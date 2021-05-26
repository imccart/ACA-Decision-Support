# Meta --------------------------------------------------------------------

## Date Created:  3/4/2020
## Date Edited:   3/17/2021
## Description:   Simple regressions of dominated choices


# Initial Regressions -----------------------------------------------------

mod1 <- feols(dominated_choice ~ assisted + english + spanish +
                FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
                perc_45to54 + perc_male + perc_asian + perc_black + perc_hispanic +
                perc_other + household_size + new_enrollee 
              | region + year + insurer, cluster="region",
              data=hh.full, weights=hh.full$ipweight)


mod2 <- feols(dominated_choice ~ assisted + english + spanish +
                FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
                perc_45to54 + perc_male + perc_asian + perc_black + perc_hispanic +
                perc_other + household_size
              | region + year + insurer, cluster="region",
              data=hh.clean, weights=hh.clean$ipweight)

dom.regs <- list("Region FE/Cluster \nAll Enrollees"=mod1, 
                 "Region FE/Cluster \nNew Enrollees"=mod2)

modelplot(dom.regs, coef_map= c("assisted"="Assistance"), facet=TRUE, size=.2) + 
  theme(strip.text.y=element_blank()) +
  scale_x_continuous(limits=c(-0.020,0.001)) +
  ggsave("figures/dominated_choice_regression.png")
  
  
modelsummary(list("1"=mod1, "2"=mod2), 
             coef_rename=c("assisted"="Assistance",
                           "english"="English Speaking HH",
                           "spanish"="Spanish Speacking HH",
                           "FPL" = "Poverty Line",
                           "perc_0to17" = "Share of HH < 18",
                           "perc_18to25" = "Share of HH 18 to 25",
                           "perc_26to34" = "Share of HH 26 to 34",
                           "perc_35to44" = "Share of HH 35 to 44",
                           "perc_45to54" = "Share of HH 45 to 54",
                           "perc_male" = "Share of HH Male",
                           "perc_asian" = "Share of HH Asian",
                           "perc_black" = "Share of HH Black",
                           "perc_hispanic" = "Share of HH Hispanic",
                           "perc_other" = "Share of HH Other",
                           "household_size" = "HH Size",
                           "new_enrollee" = "New Enrollee"),
             statistic=c('conf.int',
                         '({std.error})'),
             gof_omit = 'AIC|BIC|R2 Adj.|R2 Within|R2 Within Adj.|R2 Pseudo|Log.Lik.',
             output="latex") %>%
  kable_styling(latex_options=c("HOLD_position")) %>%
  save_kable("tables/dominated_choice_regression.tex")



# Potential outcomes approach ---------------------------------------------

hh.nest <- hh.clean %>% filter(!is.na(dominated_choice)) %>%
  nest(-c(region,year))

## overall estimates
mod.full <- hh.nest %>%
  mutate(data.est=map(data,~filter(., assisted==0)),
         data.oos=map(data,~filter(., assisted==1))) %>%
  select(-data) %>%
  mutate( regs = map(data.est, 
                     ~feglm(dominated_choice ~ english + spanish +
                              FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
                              perc_45to54 + perc_male + perc_asian + perc_black + perc_hispanic +
                              perc_other + household_size | insurer,
                            data=., family="binomial", weights=~ipweight)),
          tidied = map(regs, tidy),
          pred = map2(regs, data.oos, ~augment(.x, newdata = .y, type.predict="response")))


## bootstrap standard errors
max.boot <- 200
bs.hh.data <- bootstraps(hh.nest, times=max.boot)

bootsrp.dom <- function(j) {
  bs.dom.dat <- analysis(bs.hh.data$splits[[j]]) %>% 
    unnest(data) %>%
    nest(-c(region,year))
  
  bs.dom.run <- bs.dom.dat %>%
    mutate(data.est=map(data,~filter(., assisted==0)),
           data.oos=map(data,~filter(., assisted==1))) %>%
    select(-data) %>%
    mutate( regs = map(data.est, 
                       ~feglm(dominated_choice ~ english + spanish +
                                FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
                                perc_45to54 + perc_male + perc_asian + perc_black + perc_hispanic +
                                perc_other + household_size | insurer,
                              data=., family="binomial", weights=~ipweight)),
            tidied = map(regs, tidy),
            pred = map2(regs, data.oos, ~augment(.x, newdata = .y, type.predict="response")))
  
  bs.pred <- bs.dom.run %>% unnest(pred) %>%
    select(pred_dominated=.fitted, year, region, channel_detail, dominated_choice) %>%
    group_by(channel_detail) %>%
    summarize(tot_dominated=sum(dominated_choice, na.rm=TRUE),
              pred_dominated=sum(pred_dominated, na.rm=TRUE),
              tot_choices=n())
  
  bs.coef <- bs.dom.run %>% unnest(tidied) %>% select(year, region, term, estimate, std.error, statistic, p.value)
  
  return(list("coef"=bs.coef, "pred"=bs.pred))
}

sim.bs <- lapply(1:max.boot, bootsrp.dom) 

bs.coef <- sim.bs[[1]]$coef %>%
  mutate(boot=1)
bs.pred <- sim.bs[[1]]$pred %>%
  mutate(boot=1)
for (i in 2:max.boot) {
  bs.coef <- bind_rows(bs.coef, sim.bs[[i]]$coef %>% mutate(boot=i))
  bs.pred <- bind_rows(bs.pred, sim.bs[[i]]$pred %>% mutate(boot=i))  
}



# Summarize results -------------------------------------------------------

dom.predict <- mod.full %>% unnest(pred) %>%
  filter(!is.na(dominated_choice)) %>%
  select(obs_dominated=dominated_choice, pred_dominated=.fitted, channel_detail)

dom.any <- dom.predict %>%
  summarize(obs_dominated=sum(obs_dominated, na.rm=TRUE), 
            pred_dominated=sum(pred_dominated, na.rm=TRUE),
            tot_choices=n()) %>%
  mutate(e_y1=obs_dominated/tot_choices,
         e_y0=pred_dominated/tot_choices,
         att=e_y1-e_y0,
         att_rel=att/e_y0) %>%
  mutate(channel_detail="Any Assistance") 

dom.channel <- dom.predict %>%
  group_by(channel_detail) %>%
  summarize(obs_dominated=sum(obs_dominated, na.rm=TRUE), 
            pred_dominated=sum(pred_dominated, na.rm=TRUE),
            tot_choices=n()) %>%
  mutate(e_y1=obs_dominated/tot_choices,
         e_y0=pred_dominated/tot_choices,
         att=e_y1-e_y0,
         att_rel=att/e_y0)

dom.est <- bind_rows(dom.any, dom.channel)


bs.dom.boot.any <- bs.pred %>%   
  group_by(boot) %>%
  summarize(obs_dominated=sum(tot_dominated, na.rm=TRUE), 
            pred_dominated=sum(pred_dominated, na.rm=TRUE),
            tot_choices=sum(tot_choices, na.rm=TRUE)) %>%
  mutate(e_y1=obs_dominated/tot_choices,
         e_y0=pred_dominated/tot_choices,
         att=e_y1-e_y0,
         att_rel=att/e_y0)

bs.summary <- bs.dom.boot.any %>% ungroup() %>%
  summarize(p01=quantile(att, 0.01),
            p05=quantile(att, 0.05),
            p95=quantile(att, 0.95),
            p99=quantile(att, 0.99)) %>%
  mutate(channel_detail="Any Assistance")

bs.channel.summary <- bs.pred %>%
  group_by(channel_detail) %>%
  mutate(e_y1=tot_dominated/tot_choices,
         e_y0=pred_dominated/tot_choices,
         att=e_y1-e_y0,
         att_rel=att/e_y0) %>%
  summarize(p01=quantile(att, 0.01),
            p05=quantile(att, 0.05),
            p95=quantile(att, 0.95),
            p99=quantile(att, 0.99))

bs.ci <- bind_rows(bs.summary, bs.channel.summary)

dom.final <- dom.est %>% left_join(bs.ci, by="channel_detail")

dom.graph <- dom.final %>%
  ggplot(aes(x=factor(channel_detail, level=c("Any Assistance", "Agent", "Navigator")),y=att)) +
  geom_hline(aes(yintercept=0),linetype="dashed") +
  geom_errorbar(aes(ymin=p05, ymax=p95),
                lwd=1, width=0, position=position_dodge(width=0.5)) +
  geom_text(aes(label=paste("Base of:",round(e_y0,3), sep="\n"), y=p95+0.001)) +
  labs(
    y="Estimate and \n95% Confidence Interval",
    x="Decision Assistance") +
  geom_point(size=2,position=position_dodge(width=0.5)) +
  theme_bw() + ggsave("figures/dom_choice.png")


          