# Meta --------------------------------------------------------------------

## Date Created:  10/11/2019
## Date Edited:   3/17/2021
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
    y="Relative Frequency",
    title="Metal Tier by Type of Assistance"
  ) + theme_bw() +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Greys") + ggsave("figures/metal_stack_all.png")

ggplot(hh.full) +
  geom_bar(mapping=aes(x=channel,fill=metal),
           position="fill", color="black", size=.1) +
  labs(
    x=" ",
    y="Relative Frequency",
    title="Metal Tier by Assistance"
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
    y="Relative Frequency",
    title="Selected Insurer"
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
    y="Relative Frequency",
    title="Selected Insurer"
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

# Summary values for paper ------------------------------------------------

summary(panel.length$year_obs)
mean.dom.choice <- mean(hh.clean$dominated_choice, na.rm=T)
tot.enroll <- sum(enrollee.count.all$enroll_count)*1000
tot.enroll.new <- sum(enrollee.count.new$enroll_count)*1000
unique.enroll <- nrow(hh.full %>% distinct(household_id))
first.year <- min(enrollee.count$year)
last.year <- max(enrollee.count$year)

