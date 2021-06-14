# Preliminaries -----------------------------------------------------------
afford.threshold <- tibble(
  cutoff = c(0.08, 0.0805, 0.0813, 0.0816, 0.0805, 0.0830),
  year = c(2014, 2015, 2016, 2017, 2018, 2019)
)

## specify time periods and regions for estimation
unique.region <- hh.clean %>% distinct(region)
time.dat <- 2014:2019
area.dat <- as.list(unique.region)$region



# Estimation --------------------------------------------------------------

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
          tidied = map(regs, tidy))

for (i in 1:nrow(choice.regs)) {
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
              pred_purchase=sum(pred_purchase, na.rm=TRUE)) %>%
    mutate(region=choice.regs$region[[i]],
           year = choice.regs$year[[i]])
  
  if (i==1) {
    all.prob <- treated.dat
  } else {
    all.prob <- bind_rows(all.prob, treated.dat)
  }
  
}



# Bootstrap standard errors -----------------------------------------------

bs.choice.data <- full.nest %>% 
  mutate(hh.final = map(data_full, ~.x %>% distinct(.x$household_number)))

poss.tidy <- possibly(.f = tidy, otherwise=NULL)

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
  
  print(paste0("finished bootstrap: ", b, "at time ", Sys.time()))
} 
    
