# Preliminaries -----------------------------------------------------------
afford.threshold <- tibble(
  cutoff = c(0.08, 0.0805, 0.0813, 0.0816, 0.0805, 0.0830),
  year = c(2014, 2015, 2016, 2017, 2018, 2019)
)

## specify time periods and regions for estimation
unique.region <- hh.clean %>% distinct(region)
time.dat <- 2014:2019
area.dat <- as.list(unique.region)$region



# Estimate with purrr -----------------------------------------------------

hh.nest <- hh.clean %>%
  left_join(afford.threshold, by="year") %>%
  nest(-c(region, year))

plan.nest <- plan_data %>%
  rename(year=Year) %>%
  nest(-c(region, year))

region.year <- as_tibble(merge(time.dat, area.dat)) %>%
  rename(region=y, year=x)

source('analysis/decision-support/choice_data_function_map.R')
source('analysis/decision-support/choice_est_function_map.R')
full.nest <- region.year %>%
  left_join(hh.nest, by=c("region","year")) %>%
  rename(hh_nest=data) %>%
  left_join(plan.nest, by=c("region", "year")) %>%
  rename(plan_nest=data) %>%
  group_by(region, year) %>%
  mutate(data_full = map2(hh_nest, plan_nest, ~choice.data.fnc2(hhs=.x, plans=.y)),
         data.est=map(data_full,~filter(., assisted==0)),
         data.oos=map(data_full,~filter(., assisted==1))) 

choice.regs <- full.nest %>%
  mutate(nest_names = map(data.est, ~unique(.x$plan_name)),
         nest1=map(nest_names, ~.x[.x !="Uninsured"])) %>%
  mutate( regs = map2(data.est, nest1, ~dchoice.reg(d=.x, nest.names=.y)),
          tidied = map(regs, tidy),
          pred = data.est)

for (i in nrow(choice.regs)) {
  oos.nest <- mlogit.data(choice.regs$data_full[[i]], choice="choice", shape="long", chid.var = "household_number", alt.var="plan_name")
  nest.coef <- choice.regs$regs[[i]]
  nest.names <- choice.regs$nest1[[i]]
  nested.pred <- predict(nest.coef, newdata=oos.nest)  
  nested.pred <- as_tibble(nested.pred, rownames="household_number") %>% mutate(household_number=as.numeric(household_number))
  nested.pred <- nested.pred %>% pivot_longer(!household_number, names_to="plan_name", values_to="pred_purchase")
  treated.dat <- choice.regs$data.oos[[i]] %>%
    left_join(nested.pred, by=c("household_number", "plan_name")) %>%
    group_by(plan_name) %>% 
    summarize(tot_nonmiss=sum(!is.na(pred_purchase)),
              obs_purchase=sum(choice, na.rm=TRUE),
              pred_purchase=sum(pred_purchase, na.rm=TRUE))
  
  choice.regs$pred[[i]] <- treated.dat
}


## start back here trying the bootstrap with purrr
bs.choice.data <- full.nest %>% 
  mutate(hh.final = map(data_full, ~.x %>% distinct(.x$household_number)))

max.boot <- 200
for (b in 1:max.boot) {  
  bs.choice.run <- bs.choice.data %>%
    mutate(hh.est = map(hh.final, ~slice_sample(.x, n=nrow(.x), replace=TRUE) %>%
                   mutate(new_hh_number=cur_group_rows()) %>%
                   rename(household_number=`.x$household_number`)),
           data.bs = map2(data_full, hh.est, ~.x %>% inner_join(.y, by=c("household_number")) %>%
                            mutate(household_number=new_hh_number)),
           data.est = map(data.bs,~filter(., assisted==0)),
           plans.est = map(data.est, ~.x %>% distinct(plan_name)),
           data.oos = map2(data.bs, plans.est, ~.x %>% filter(assisted==1) %>%
                            inner_join(.y, by=c("plan_name"))),
           nest_names = map(data.est, ~unique(.x$plan_name)),
           nest1 = map(nest_names, ~.x[.x !="Uninsured"]),
           regs = map2(data.est, nest1, ~dchoice.reg(d=.x, nest.names=.y)),
           tidied = map(regs, tidy)) %>%
    select(-c(data_full, hh_nest, plan_nest, hh.final))
    
  for (i in 1:nrow(bs.choice.run)) {
    oos.nest <- dfidx(bind_rows(bs.choice.run$data.oos[[i]], bs.choice.run$data.est[[i]]), shape="long", idx = c("household_number","plan_name"))
    nest.coef <- bs.choice.run$regs[[i]]
    nest.names <- bs.choice.run$nest1[[i]]
    test <- is.error(predict(nest.coef, newdata=oos.nest))
    if (test==FALSE) {
      nested.pred <- predict(nest.coef, newdata=oos.nest)
      nested.pred <- as_tibble(nested.pred, rownames="household_number") %>% mutate(household_number=as.numeric(household_number))
      nested.pred <- nested.pred %>% pivot_longer(!household_number, names_to="plan_name", values_to="pred_purchase")
    } else {
      nested.pred <- bs.choice.run$data.oos[[i]] %>% select(household_number, plan_name) %>% mutate(pred_purchase=NA)
    }
    treated.dat <- bs.choice.run$data.oos[[i]] %>%
      left_join(nested.pred, by=c("household_number", "plan_name")) %>%
      group_by(plan_name) %>% 
      summarize(tot_nonmiss=sum(!is.na(pred_purchase)),
                obs_purchase=sum(choice, na.rm=TRUE),
                pred_purchase=sum(pred_purchase, na.rm=TRUE)) %>%
      mutate(region=bs.choice.run$region[[i]],
             year = bs.choice.run$year[[i]])
    
    if (i==1) {
      final.pred <- treated.dat
    } else {
      final.pred <- bind_rows(final.pred, treated.dat)
    }
  }
  
  bs.pred <- final.pred %>%
    group_by(plan_name) %>%
    summarize(tot_nonmiss=sum(tot_nonmiss, na.rm=TRUE),
              obs_purchase=sum(obs_purchase, na.rm=TRUE),
              pred_purchase=sum(pred_purchase, na.rm=TRUE)) %>%
    mutate(boot=b)
  
  bs.coef <- bs.choice.run %>% unnest(tidied) %>% select(year, region, term, estimate, std.error, statistic, p.value) %>%
    mutate(boot=b)
  
  if (b==1) {
    sim.bs.coef <- bs.coef
    sim.bs.pred <- bs.pred
  } else {
    sim.bs.coef <- bind_rows(sim.bs.coef, bs.coef)
    sim.bs.pred <- bind_rows(sim.bs.pred, bs.pred)  
  }
  
} 
    




          




    










# # Estimate model ----------------------------------------------------------
# source('analysis/decision-support/choice_data_function.R')
# source('analysis/decision-support/choice_est_function.R')
# source('analysis/decision-support/choice_est_bs_function.R')
# 
# 
# for (t in time.dat) {
#   
#   step=0
#   for (r in area.dat) {
#     step=step+1
#     
#     choice.data <- choice.data.fnc(t=t,r=r)
#     assign(paste0("est.data.",r,".",t), choice.data$est.data)
#     assign(paste0("oos.data.",r,".",t), choice.data$oos.data)
#     nest.names <- unique(choice.data$est.data$plan_name)
#     nest.in <- nest.names[nest.names != "Uninsured"]
#     nest.out <- nest.names[nest.names == "Uninsured"]
#     assign(paste0("nest.in.",r,".",t), nest.in)
#     assign(paste0("nest.out.",r,".",t), nest.out)
#     
#     choice.est <- dchoice.est(d=choice.data$est.data, oos=choice.data$oos.data, t=t, r=r)
#     
#     treated.dat <- choice.est$pred
#     coef.vals <- choice.est$coef
#     if (step==1) {
#       final.data <- treated.dat
#       final.coef <- coef.vals
#     } else {
#       final.data <- bind_rows(final.data, treated.dat)
#       final.coef <- bind_rows(final.coef, coef.vals)
#     }
#   }
#   
#   assign(paste0("final.data.",t),final.data)
#   assign(paste0("final.coef.",t),final.coef)
#   textme(msg=paste0(emo::ji("space"),"One small step for man...You've completed time: ",t))
#   
# }
# 
# 
# step=0
# for (t in time.dat) {
#   step=step+1
#   if (step==1) {
#     est.prob <- get(paste0("final.data.",t))
#     bs.starting <- get(paste0("final.coef.",t))
#   } else {
#     est.prob <- bind_rows(est.prob, get(paste0("final.data.",t)))
#     bs.starting <- bind_rows(bs.starting, get(paste0("final.coef.",t)))
#   }
# }
# 
# all.coef <- bs.starting
# all.prob <- est.prob
# 
# 
# # Bootstrap Standard Errors -----------------------------------------------
# max.boot <- 200
# 
# for (b in 1:max.boot) {
# 
#   for (t in time.dat) {
#     
#     step=0
#     for (r in area.dat) {
#       step=step+1
#       
#       nest.in <- get(paste0("nest.in.",r,".",t))
#       nest.out <- get(paste0("nest.out.",r,".",t))
#       
#       bs.choice <- dchoice.bs(t=t, r=r)
#       treated.dat <- bs.choice$pred
#       coef.vals <- bs.choice$coef
# 
#       if (step==1) {
#         boot.data <- treated.dat
#         boot.coef <- coef.vals
#       } else {
#         boot.data <- bind_rows(boot.data, treated.dat)
#         boot.coef <- bind_rows(boot.coef, coef.vals)
#       }
#     }
#     
#     assign(paste0("boot.data.",t),boot.data)
#     assign(paste0("boot.coef.",t),boot.coef)
#     
#   }
#   
#   step=0
#   for (t in time.dat) {
#     step=step+1
#     if (step==1) {
#       boot.data.time <- get(paste0("boot.data.",t))
#       boot.coef.time <- get(paste0("boot.coef.",t))
#     } else {
#       boot.data.time <- bind_rows(boot.data.time, get(paste0("boot.data.",t)))
#       boot.coef.time <- bind_rows(boot.coef.time, get(paste0("boot.coef.",t)))
#     }
#   }
#   
#   if (b==1) {
#     bs.coef <- boot.coef.time %>%
#       mutate(boot=1)
#     bs.pred <- boot.data.time %>%
#       mutate(boot=1)
#   }
#   if (b>1) {
#     bs.coef <- bind_rows(bs.coef, boot.coef.time %>% mutate(boot=b))
#     bs.pred <- bind_rows(bs.pred, boot.data.time %>% mutate(boot=b))  
#   }
#   
#   textme(msg=paste0(emo::ji("boot"),"You get the boot! You've completed iteration: ",b))
# }
# 
# textme(msg=paste0(emo::ji("mage"),"  As it is written, so it has been done.  ",emo::ji("sparkles")))