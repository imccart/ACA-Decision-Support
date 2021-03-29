# Meta --------------------------------------------------------------------

## Date Created:  3/4/2020
## Date Edited:   3/17/2021
## Description:   Simple regressions of dominated choices


# Initial Regressions -----------------------------------------------------

mod1 <- feols(switch ~ assisted + english + spanish +
                FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
                perc_45to54 + perc_male + perc_asian + perc_black + perc_hispanic +
                perc_other + household_size
              | region + year + insurer, cluster="region",
              data=hh.full)

mod2 <- feols(switch ~ assisted + english + spanish +
                FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
                perc_45to54 + perc_male + perc_asian + perc_black + perc_hispanic +
                perc_other + household_size
             | household_id + year + insurer, cluster="household_id", 
             data=hh.full)

mod3 <- feols(dominated_choice ~ assisted + english + spanish +
                FPL + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
                perc_45to54 + perc_male + perc_asian + perc_black + perc_hispanic +
                perc_other + household_size
              | region + year + insurer, cluster="region",
              data=hh.full %>% filter(switch==1))


dom.regs <- list("Switching \nRegion FE"=mod1, 
                 "Switching \nHH FE"=mod2, 
                 "Dominated choice \namong switchers"=mod3)

modelplot(dom.regs, coef_map= c("assisted"="Assistance"), facet=TRUE, size=.1) + 
  theme(strip.text.y=element_blank()) +
  scale_x_continuous(limits=c(-0.050,0.3)) +
  ggsave("figures/switching_regression.png")
  
  
modelsummary(list("1"=mod1, "2"=mod2, "3"=mod3), 
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
                           "household_size" = "HH Size"),
             statistic=c('conf.int',
                         '({std.error})'),
             gof_omit = 'AIC|BIC|R2 Adj.|R2 Within|R2 Within Adj.|R2 Pseudo|Log.Lik.',
             output="latex") %>%
  kable_styling(latex_options=c("HOLD_position")) %>%
  save_kable("tables/switching_regression.tex")
