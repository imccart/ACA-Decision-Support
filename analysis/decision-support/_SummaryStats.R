# Meta --------------------------------------------------------------------

## Date Created:  10/11/2019
## Date Edited:   5/3/2021
## Description:   Basic summary stats


# Summary Statistics ------------------------------------------------------

## metal by type of assistance (all enrollees)
metal.assist.all <- hh.full %>% group_by(channel_detail, metal) %>%
  summarize(metal_count=n()) %>%
  mutate(metal_pct=metal_count/sum(metal_count),
         metal_pct=round(metal_pct,3))

## metal by type of assistance (new enrollees)
metal.assist.new <- hh.clean %>%
  group_by(channel_detail, metal) %>%
  summarize(metal_count=n()) %>%
  mutate(metal_pct=metal_count/sum(metal_count),
         metal_pct=round(metal_pct))


## table of assistance and tier choice
kable(metal.assist.all[,2:4], format="latex",
                       col.names=c("Type of Assistance"," Count", "Percent"),
                       format.args=list(big.mark=","),
                       booktabs=T) %>%
  kable_styling(latex_options=c("HOLD_position")) %>%
  group_rows("Agent or Broker", 1, 5) %>%
  group_rows("Navigator", 6, 10) %>%
  group_rows("Unassisted", 11, 15) %>%
  save_kable("tables/metal_assistance.tex")

## figure of assistance and tier choice
ggplot(hh.full) +
  geom_bar(mapping=aes(x=channel_detail,fill=metal),
           position="fill", color="black", size=.1) +
  labs(
    x="Type of Assistance",
    y="Relative Frequency"
  ) + theme_bw() +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Greys") + ggsave("figures/metal_stack_all.png")

ggplot(hh.full) +
  geom_bar(mapping=aes(x=channel,fill=metal),
           position="fill", color="black", size=.1) +
  labs(
    x=" ",
    y="Relative Frequency"
  ) + theme_bw() +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Greys") + ggsave("figures/metal_stack_any.png")


## figure of assistance and insurer
hh.full %>% mutate(insurer = case_when(
  insurer=="Blue_Shield" ~ "BCBS",
  insurer=="Health_Net" ~ "HealthNet",
  insurer=="Anthem" ~ "Anthem",
  insurer %in% c("Chinese_Community", "Contra_Costa", "Molina", "LA_Care","Oscar","SHARP","United","Valley","Western") ~ "Other",
  insurer=="Kaiser" ~ "Kaiser",
  TRUE ~ "Missing")) %>%
  ggplot() + 
  geom_bar(mapping=aes(x=channel_detail,fill=insurer),
           position="fill", color="black", size=.1) +
  labs(
    x="Type of Assistance",
    y="Relative Frequency"
  ) + theme_bw() +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Greys") + ggsave("figures/insurer_stack_all.png")


hh.full %>% mutate(insurer = case_when(
  insurer=="Blue_Shield" ~ "BCBS",
  insurer=="Health_Net" ~ "HealthNet",
  insurer=="Anthem" ~ "Anthem",
  insurer %in% c("Chinese_Community", "Contra_Costa", "Molina", "LA_Care","Oscar","SHARP","United","Valley","Western") ~ "Other",
  insurer=="Kaiser" ~ "Kaiser",
  TRUE ~ "Missing")) %>%
  ggplot() + 
  geom_bar(mapping=aes(x=channel,fill=insurer),
           position="fill", color="black", size=.1) +
  labs(
    x=" ",
    y="Relative Frequency"
  ) + theme_bw() +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Greys") + ggsave("figures/insurer_stack_any.png")


## data on enrollees per year
enrollee.count.all <- hh.full %>% count(year) %>%
  rename(enroll_count=n) %>%
  mutate(Type="All Enrollees")
enrollee.count.new <- hh.clean %>%
  count(year) %>% rename(enroll_count=n) %>%
  mutate(Type="New Enrollees")
enrollee.count <- bind_rows(enrollee.count.all, enrollee.count.new) %>%
  mutate(enroll_count=enroll_count/1000)

## graph of enrollees per year
ggplot() + geom_line(data=enrollee.count, aes(x=year,y=enroll_count, linetype=Type),
                     show.legend=FALSE) +
  geom_text(data=enrollee.count %>% filter(year==last(year)), 
            aes(label=Type, x=year-1, y=enroll_count),
            vjust=-.5,
            show.legend=FALSE) +
  labs(
    x="Year",
    y="Enrollees \n(1000s)"
  ) +
  scale_y_continuous(limits=c(500,1500), labels=comma) +
  theme_bw() + ggsave("figures/enrollee_count.png")

## count of years same household is observed
panel.length <- hh.full %>%
  group_by(household_id) %>% 
  summarize(year_obs=n()) %>%
  arrange(year_obs)



# Balance among treatment and control -------------------------------------

# estimate propensity scores
for (t in 2014:2019) {
  logit.full <- glm(assisted ~ FPL + perc_0to17 + perc_18to25 + perc_65plus + 
                      perc_black + perc_hispanic + perc_asian + perc_male + household_size,
                    data=hh.full %>% filter(year==t),
                    family='binomial')
  logit.clean <- glm(assisted ~ FPL + perc_0to17 + perc_18to25 + perc_65plus + 
                       perc_black + perc_hispanic + perc_asian + perc_male + household_size,
                     data=hh.clean %>% filter(year==t),
                     family='binomial')
  
  hh.full <- hh.full %>% add_predictions(logit.full, type="response", var=paste0("pred_assist_",t))
  hh.clean <- hh.clean %>% add_predictions(logit.full, type="response", var=paste0("pred_assist_",t))
}

hh.full <- hh.full %>%
  mutate(pred_assist=case_when(
    year==2014 ~ pred_assist_2014,
    year==2015 ~ pred_assist_2015,
    year==2016 ~ pred_assist_2016,
    year==2017 ~ pred_assist_2017,
    year==2018 ~ pred_assist_2018,
    year==2019 ~ pred_assist_2019,
    TRUE ~ NA_real_
  )) %>%
  select(-starts_with("pred_assist_")) %>%
  mutate(ipweight=case_when(
    assisted==1 ~ 1,
    assisted==0 ~ pred_assist/(1-pred_assist),
    TRUE ~ NA_real_
  ))

hh.clean <- hh.clean %>%
  mutate(pred_assist=case_when(
    year==2014 ~ pred_assist_2014,
    year==2015 ~ pred_assist_2015,
    year==2016 ~ pred_assist_2016,
    year==2017 ~ pred_assist_2017,
    year==2018 ~ pred_assist_2018,
    year==2019 ~ pred_assist_2019,
    TRUE ~ NA_real_
  )) %>%
  select(-starts_with("pred_assist_")) %>%
  mutate(ipweight=case_when(
    assisted==1 ~ 1,
    assisted==0 ~ pred_assist/(1-pred_assist),
    TRUE ~ NA_real_
  ))

ggplot(hh.full, aes(x=pred_assist)) + 
  geom_histogram() + 
  facet_wrap(~ifelse(assisted==1,"Any Assistance","No Assistance"), ncol=1) +
  labs(
    x="Propensity Score",
    y="Count of Households \n(1000s)"
    ) +
  scale_y_continuous(limits = c(0,1000000),
                     breaks=c(0,250000,500000,750000,1000000),
                     labels=c("0","250","500","750","1,000")) +
  theme_bw() + ggsave("figures/ps_assist_full.png")


ggplot(hh.clean, aes(x=pred_assist)) + 
  geom_histogram() + 
  facet_wrap(~ifelse(assisted==1,"Any Assistance","No Assistance"), ncol=1) +
  labs(
    x="Propensity Score",
    y="Count of Households \n(1000s)"
  ) +
  scale_y_continuous(limits = c(0,750000),
                     breaks=c(0,250000,500000,750000),
                     labels=c("0","250","500","750")) +
  theme_bw() + ggsave("figures/ps_assist_clean.png")



set.cobalt.options(binary="std")
new.names <- c(FPL = "Federal Poverty Level",
               perc_0to17 = "Percent below 18",
               perc_18to25 = "Percent 18 to 25",
               perc_65plus = "Percent 65+",
               perc_black = "Percent Black",
               perc_hispanic = "Percent Hispanic",
               perc_asian = "Percent Asian",
               perc_male = "Percent Male",
               household_size = "HH Size")

covs <- hh.full %>% select(FPL, perc_0to17, perc_18to25, perc_65plus, 
                   perc_black, perc_hispanic, perc_asian, perc_male, 
                   household_size)

love.plot(bal.tab(covs, treat = hh.full$assisted, weights = hh.full$ipweight),
          data=hh.full,
          estimand="ATT",
          stats=c("mean.diffs", "ks.statistics"),
          var.order="unadjusted",
          abs=TRUE,
          line=FALSE,
          thresholds=c(m=0.1, ks=0.05),
          var.names=new.names,
          colors=c("black", "gray"),
          shapes=c("circle filled", "circle"),
          sample.names=c("Unweighted", "IPW-ATT"),
          limits=list(m=c(0,.25),
                      ks=c(0,.25)),
          wrap=20,
          position="bottom",
          title=NULL)
ggsave("figures/cov_balance.png")
  
# Summary values for paper ------------------------------------------------

summary(panel.length$year_obs)
mean.dom.choice <- mean(hh.clean$dominated_choice, na.rm=T)
tot.enroll <- sum(enrollee.count.all$enroll_count)
tot.enroll.new <- sum(enrollee.count.new$enroll_count)
first.year <- min(enrollee.count$year)
last.year <- max(enrollee.count$year)



# # Summary tables ----------------------------------------------------------

## household summary statistics
sum.stats.data <- hh.full %>% ungroup() %>%
  mutate(FPL_low=if_else(FPL_bracket=="138orless",1,0),
         FPL_250=if_else(FPL_bracket=="138to250",1,0),
         FPL_400=if_else(FPL_bracket=="250to400",1,0),
         FPL_high=if_else(FPL_bracket=="400ormore",1,0),
         Kaiser=if_else(insurer=="Kaiser" & !is.na(plan_number_nocsr),1,0),
         Anthem=if_else(insurer=="Anthem" & !is.na(plan_number_nocsr),1,0),
         BlueShield=if_else(insurer=="Blue_Shield" & !is.na(plan_number_nocsr),1,0),
         HealthNet=if_else(insurer=="Health_Net" & !is.na(plan_number_nocsr),1,0),
         Uninsured=if_else(is.na(plan_number_nocsr),1,0),
         Other=if_else( (!insurer %in% c("Kaiser","Anthem","Blue_Shield","Health_Net")) & !is.na(plan_number_nocsr),1,0),
         Bronze=if_else(metal=="Bronze" & !is.na(plan_number_nocsr),1,0),
         Silver=if_else(metal=="Silver" & !is.na(plan_number_nocsr),1,0),
         Gold=if_else(metal=="Gold" & !is.na(plan_number_nocsr),1,0),         
         Platinum=if_else(metal=="Platinum" & !is.na(plan_number_nocsr),1,0),
         Catastrophic=if_else(metal=="Catastrophic" & !is.na(plan_number_nocsr),1,0),
         HMO=if_else(plan_network_type=="HMO" & !is.na(plan_number_nocsr),1,0),
         PPO=if_else(plan_network_type=="PPO" & !is.na(plan_number_nocsr),1,0),
         EPO=if_else(plan_network_type=="EPO" & !is.na(plan_number_nocsr),1,0),
         HSP=if_else(plan_network_type=="HSP" & !is.na(plan_number_nocsr),1,0)) %>%
  select(starts_with("FPL_"),Kaiser, Anthem, BlueShield, HealthNet, Uninsured, Other, Bronze, Silver, Gold, Platinum, Catastrophic, 
         HMO, PPO, EPO, HSP, assisted, navigator, any_agent, household_size, num_children_subject, perc_black, perc_hispanic, perc_white)
  

sum.stats.assist <- sum.stats.data %>% filter(assisted==1) %>%
  summarize(across(c("household_size","num_children_subject", "perc_black", "perc_hispanic", "perc_white",
                     "FPL_low","FPL_250","FPL_400","FPL_high","Kaiser","Anthem","BlueShield","HealthNet",
                     "Other","Bronze","Gold","Silver","Platinum","Catastrophic","HMO","PPO","EPO","HSP"),
                   list(Mean=mean, N=~n(),
                    q1=~quantile(., probs=0.10, na.rm=TRUE),
                    q9=~quantile(., probs=0.90, na.rm=TRUE)),
            na.rm=TRUE,
            .names="{col}_{fn}"))

sum.stats.unassist1 <- sum.stats.data %>% filter(assisted==0) %>%
  summarize(across(c("household_size","num_children_subject", "perc_black", "perc_hispanic", "perc_white",
                     "FPL_low","FPL_250","FPL_400","FPL_high"),
                   list(Mean=mean, N=~n(),
                        q1=~quantile(., probs=0.10, na.rm=TRUE),
                        q9=~quantile(., probs=0.90, na.rm=TRUE)),
                   na.rm=TRUE,
                   .names="{col}_{fn}"))
sum.stats.unassist2 <- sum.stats.data %>% filter(assisted==0, Uninsured==0) %>%
  summarize(across(c("Kaiser","Anthem","BlueShield","HealthNet",
                     "Other","Bronze","Gold","Silver","Platinum","Catastrophic","HMO","PPO","EPO","HSP"),
                   list(Mean=mean, N=~n(),
                        q1=~quantile(., probs=0.10, na.rm=TRUE),
                        q9=~quantile(., probs=0.90, na.rm=TRUE)),
                   na.rm=TRUE,
                   .names="{col}_{fn}"))



sum.stats.all <- sum.stats.data %>%
  summarize(across(c("household_size","num_children_subject", "perc_black", "perc_hispanic", "perc_white",
                     "FPL_low","FPL_250","FPL_400","FPL_high","Kaiser","Anthem","BlueShield","HealthNet",
                     "Uninsured","Other","Bronze","Gold","Silver","Platinum","Catastrophic","HMO","PPO","EPO","HSP"),
                   list(Mean=mean, N=~n(),
                        q1=~quantile(., probs=0.10, na.rm=TRUE),
                        q9=~quantile(., probs=0.90, na.rm=TRUE)),
                   na.rm=TRUE,
                   .names="{col}_{fn}"))

tot.count <- nrow(sum.stats.data)/1000000
assist.count <- nrow(sum.stats.data %>% filter(assisted==1))/1000000
unassist.count <- nrow(sum.stats.data %>% filter(assisted==0))/1000000
tot.unins <- nrow(sum.stats.data %>% filter(Uninsured==1))/1000000
mean.stats.all <- sum.stats.all %>% select(ends_with("_Mean")) %>%
  pivot_longer(cols=ends_with("_Mean"),
               values_to="Overall") %>%
  mutate(name=str_remove(name,"_Mean")) %>%
  bind_rows(as_tibble(tot.count) %>% mutate(name="Obs") %>% rename(Overall=value))
mean.stats.assist <- sum.stats.assist %>% select(ends_with("_Mean")) %>%
  pivot_longer(cols=ends_with("_Mean"),
               values_to="Assisted") %>%
  mutate(name=str_remove(name,"_Mean")) %>%
  bind_rows(as_tibble(assist.count) %>% mutate(name="Obs") %>% rename(Assisted=value))
mean.stats.unassist1 <- sum.stats.unassist1 %>% select(ends_with("_Mean")) %>%
  pivot_longer(cols=ends_with("_Mean"),
               values_to="Unassisted") %>%
  mutate(name=str_remove(name,"_Mean"))
mean.stats.unassist2 <- sum.stats.unassist2 %>% select(ends_with("_Mean")) %>%
  pivot_longer(cols=ends_with("_Mean"),
               values_to="Unassisted") %>%
  mutate(name=str_remove(name,"_Mean"))
mean.stats.unassist <- mean.stats.unassist1 %>% bind_rows(mean.stats.unassist2) %>%
  bind_rows(as_tibble(unassist.count) %>% mutate(name="Obs") %>% rename(Unassisted=value))


final.sum.stats <- mean.stats.assist %>%
  left_join(mean.stats.unassist, by="name") %>%
  left_join(mean.stats.all, by="name") %>%
  bind_rows(as_tibble(tot.unins) %>% mutate(name="Uninsured") %>% rename(Overall=value) %>%
              mutate(Assisted=0) %>% mutate(Unassisted=tot.unins)) %>%
  mutate(name=case_when(
    name=="FPL_low" ~ "below 138%",
    name=="FPL_250" ~ "between 138 and 250%",
    name=="FPL_400" ~ "between 250 and 400%",
    name=="FPL_high" ~ "above 400%",
    name=="BlueShield" ~ "Blue Shield",
    name=="HealthNet" ~ "Health Net",
    name=="any_agent" ~ "Agent or Broker",
    name=="navigator" ~ "Navigator",
    name=="perc_black" ~ "Percent Black",
    name=="perc_hispanic" ~ "Percent Hispanic",
    name=="perc_white" ~ "Percent White",
    name=="household_size" ~ "Household Size",
    name=="num_children_subject" ~ "No. of Children",
    name=="Obs" ~ "Total HHs (millions)",
    name=="Uninsured" ~ "Total Uninsured (millions)",
    TRUE ~ name
  ))

options(knitr.kable.NA = '')
kable(final.sum.stats, format="latex",
      col.names = c("Variable","Assisted","Unassisted","Overall"),
      digits=c(0,2,2,2),
      booktabs=T) %>%
  pack_rows("Household Demographics", 1, 5) %>%  
  pack_rows("Income relative to FPL", 6, 9) %>%
  pack_rows("Insurer",10, 14) %>%
  pack_rows("Metal Tier",15, 19) %>%
  pack_rows("Network Type", 20, 23) %>%
  save_kable("tables/summary_stats.tex")

