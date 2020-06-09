# Meta --------------------------------------------------------------------

## Date Created:  10/11/2019
## Date Edited:   6/5/2020
## Description:   Basic summary stats


# Summary Statistics ------------------------------------------------------


## metal by type of assistance (all enrollees)
metal.assist.all <- final.data %>% group_by(channel, metal) %>%
  summarize(metal_count=n()) %>%
  mutate(metal_pct=metal_count/sum(metal_count),
         metal_pct=round(metal_pct,3))

## metal by type of assistance (new enrollees)
metal.assist.new <- final.data %>% filter(new_enrollee==1) %>%
  group_by(channel, metal) %>%
  summarize(metal_count=n()) %>%
  mutate(metal_pct=metal_count/sum(metal_count),
         metal_pct=round(metal_pct))

## table of assistance and tier choice
kable(metal.assist.all[,2:4], "latex", booktabs=T, 
      col.names=c("Tier"," Count", "Percent")) %>%
  group_rows("Insurance Agent", 1, 5) %>%
  group_rows("Other Assitance", 6, 10) %>%
  group_rows("Unassisted", 11, 15)

## figure of assistance and tier choice
stack.all <- ggplot(final.data) +
  geom_bar(mapping=aes(x=channel,fill=metal),
           position="fill", color="black") +
  labs(
    x="Type of Assistance",
    y="Relative Frequency",
    title="Metal Tier by Type of Assistance"
  ) + theme_bw() +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Greys")
ggsave("figures/metal_stack_all.png", stack.all)

## data on enrollees per year
enrollee.count.all <- final.data %>% count(year) %>%
  rename(all_count=n)
enrollee.count.new <- final.data %>% filter(new_enrollee==1) %>%
  count(year) %>% rename(new_count=n)
enrollee.count <- enrollee.count.all %>%
  left_join(enrollee.count.new, by=c("year"))

## graph of enrollees per year
enrollee.count %>%
  ggplot() + geom_line(aes(x=year,y=all_count)) +
  geom_line(aes(x=year,y=new_count)) +
  labs(
    x="Year",
    y="Enrollees"
  )

## count of years same household is observed
panel.length <- final.data %>%
  group_by(household_id) %>% 
  summarize(year_obs=n()) %>%
  arrange(year_obs)
summary(panel.length$year_obs)

# Summary values for paper ------------------------------------------------

mean.dom.choice <- mean(final.data$dominated_choice, na.rm=T)
tot.enroll <- sum(enrollee.count$all_count)
tot.enroll.new <- sum(enrollee.count$new_count)
first.year <- min(enrollee.count$year)
last.year <- max(enrollee.count$year)

# Initial Regressions -----------------------------------------------------


reg1 <- felm(dominated_choice ~ any_assist + lang_english + lang_spanish +
               low_income + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
               perc_45to54 + perc_male + perc_asian + perc_black + perc_hispanic +
               perc_other + hh_single + SEP + new_enrollee 
             | region + year + insurer | 0 | region, 
     data=final.data)

reg2 <- felm(dominated_choice ~ any_assist + lang_english + lang_spanish +
               low_income + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
               perc_45to54 + perc_male + perc_asian + perc_black + perc_hispanic +
               perc_other + hh_single + SEP + new_enrollee
             | household_id + year + insurer | 0 | household_id, 
             data=final.data)

reg1.b <- felm(dominated_choice ~ assist_agent + assist_other + lang_english + lang_spanish +
                 low_income + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
                 perc_45to54 + perc_male + perc_asian + perc_black + perc_hispanic +
                 perc_other + hh_single + SEP + new_enrollee 
               | region + year + insurer  | 0 | region, 
               data=final.data)


reg2.b <- felm(dominated_choice ~ assist_agent + assist_other + lang_english + lang_spanish +
               low_income + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
               perc_45to54 + perc_male + perc_asian + perc_black + perc_hispanic +
               perc_other + hh_single + SEP + new_enrollee
             | household_id + year + insurer | 0 | household_id, 
             data=final.data)
