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
tot.enroll <- sum(enrollee.count.all$enroll_count)*1000
tot.enroll.new <- sum(enrollee.count.new$enroll_count)*1000
unique.enroll <- nrow(hh.full %>% distinct(household_id))
first.year <- min(enrollee.count$year)
last.year <- max(enrollee.count$year)

