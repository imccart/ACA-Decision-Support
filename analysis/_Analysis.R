# Meta --------------------------------------------------------------------

## Title:         Decision Assistance and Health Insurance Choice
## Author:        Ian McCarthy & Evan Saltzman
## Date Created:  10/11/2019
## Date Edited:   10/28/2019
## Description:   Analysis file for project


# Load data ---------------------------------------------------------------

data <- get(load("data/ca_enrollment_data_AUG022019")) # individual-level data
households <- get(load("data/ca_household_data_AUG022019")) # household-level data
plan_data <- read.csv("data/ca_plan_data2.csv") # Covered California plan data
zip3_choices <- read.csv("data/zip3_choices2.csv",row.names = 1) # choice set by 3 digit zip and rating area
product_definitions <- read.csv("data/product_definitions.csv",row.names = 1) # definitions of column names in zip3_choices



# Clean Plan Data ---------------------------------------------------------

## rename insurers
plan_data <- plan_data %>%
  mutate(
    Issuer_Name = case_when(
      Issuer_Name == "Anthem Blue Cross" ~ "Anthem",
      Issuer_Name == "Blue Shield" ~ "Blue_Shield",
      Issuer_Name == "Chinese Community" ~ "Chinese_Community",
      Issuer_Name == "Contra Costa Health Plan" ~ "Contra_Costa",
      Issuer_Name == "Health Net" ~ "Health_Net",
      Issuer_Name == "LA Care" ~ "LA_Care",
      Issuer_Name == "UnitedHealthcare" ~ "United",
      Issuer_Name == "Western Health" ~ "Western",
      Issuer_Name == "Sharp" ~ "SHARP"
    )
  )


## Metal tier
plan_data <- plan_data %>%
  mutate(metal = as.character(metal_level),
         metal = replace(metal, metal %in% c("Silver - Enhanced 73",
                                             "Silver - Enhanced 87",
                                             "Silver - Enhanced 94"), 
                         "Silver") )



# Clean Individual Data ---------------------------------------------------

## Drop all uninsured records for this analysis
data.clean <- data %>%
  filter(!is.na(plan_id))

## Create Age Group Variable
data.clean <- data.clean %>%
  mutate(
    age_group=case_when(
      AGE < 18 ~ "0to17",
      AGE >= 18 & AGE < 26 ~ "18to25",
      AGE >= 26 & AGE < 35 ~ "26to34",
      AGE >= 35 & AGE < 45 ~ "35to44",
      AGE >= 45 & AGE < 55 ~ "45to54",
      AGE >= 55 & AGE < 65 ~ "55to64",
      AGE >= 65 & AGE < 120 ~ "65plus",
      TRUE ~ NA_character_
    )
  )


## Gender
data.clean <- data.clean %>%
  mutate(
    Gender = case_when(
      gender == 1 ~ "Male",
      gender == 0 ~ "Female"
    )
  )

## Metal
data.clean <- data.clean %>%
  mutate(metal = as.character(metal_level_enhanced),
         metal = replace(metal, metal %in% c("Silver - Enhanced 73",
                                             "Silver - Enhanced 87",
                                             "Silver - Enhanced 94"), 
                         "Silver"),
         metal = replace(metal, metal == "Minimum Coverage", "Catastrophic"))



## Network type
data.clean <- data.clean %>%
  separate(zip_region_year, c(NA,"region",NA), sep="_") %>%
  mutate(region = as.integer(region)) %>%
  left_join( (plan_data %>% 
                select(plan_name=Plan_Name, region, 
                       year=ENROLLMENT_YEAR, 
                       plan_network_type=PLAN_NETWORK_TYPE) %>%
                mutate(plan_name=as.character(plan_name))),
             by=c("plan_name","region","year")) %>%
  mutate(plan_network_type=as.character(plan_network_type))

## Income groups
data.clean <- data.clean %>%
  mutate(
    subsidy_fpl_bracket = case_when(
      FPL <= 1.38 ~ "138% FPL or less",
      FPL > 1.38 & FPL <= 1.50 ~ "138% FPL to 150% FPL",
      FPL > 1.50 & FPL <= 2.00 ~ "150% FPL to 200% FPL",
      FPL > 2.00 & FPL <= 2.50 ~ "200% FPL to 250% FPL",
      FPL > 2.50 & FPL <= 4.00 ~ "250% FPL to 400% FPL",
      FPL > 4.00 ~ "400% FPL or greater",
      TRUE ~ NA_character_
    )
  )

## Language
data.clean <- data.clean %>%
  mutate(
    language = case_when(
      language_spoken == "English" ~ "English",
      language_spoken == "Spanish" ~ "Spanish",
      !(language_spoken %in% c("(nonres","English","Spanish")) ~ "Other Language",
      TRUE ~ NA_character_
    )
  )

## Subsidies
data.clean <- data.clean %>%
  left_join( (households %>% select(household_id, year, subsidized_members)),
             by=c("household_id","year")) %>%
  mutate(subsidy_eligible = as.numeric(subsidized_members>0),
         subsidized = case_when(
           subsidy_eligible == 1 ~ "Subsidized",
           subsidy_eligible == 0 ~ "Unsubsidized",
           TRUE ~ NA_character_
         ),
         csr_eligible = as.numeric(subsidized == "Subsidized" & FPL <= 2.50)
  )

## Previous plan
data.clean <- data.clean %>%
  left_join( (households %>% select(household_id, year, previous_plan_number)),
             by=c("household_id","year")) 



## Dominated Plans
data.clean <- data.clean %>%
  mutate(dominated_choice = 
           case_when(
             csr_eligible == 1 & 
               subsidy_fpl_bracket %in% c("138% FPL or less","138% FPL to 150% FPL") &
               metal %in% c("Gold","Platinum") ~ 1,
             csr_eligible == 1 & 
               subsidy_fpl_bracket %in% c("150% FPL to 200% FPL") &
               metal == "Gold" ~ 1,
             csr_eligible == 1 &
               subsidy_fpl_bracket %in% c("138% FPL or less","138% FPL to 150% FPL") &
               !(metal %in% c("Gold","Platinum")) ~ 0,
             csr_eligible == 1 & 
               subsidy_fpl_bracket %in% c("150% FPL to 200% FPL") &
               metal != "Gold" ~ 0,
             TRUE ~ NA_real_
           ))
  

## CSR choose bronze_flag
data.clean <- data.clean %>%
  mutate(csr_chose_bronze = 
           case_when(
             csr_eligible == 1 & 
               subsidy_fpl_bracket %in% c("138% FPL or less","138% FPL to 150% FPL","150% FPL to 200% FPL") &
               metal %in% c("Bronze","Catastrophic") ~ 1,
             csr_eligible == 1 &
               subsidy_fpl_bracket %in% c("138% FPL or less","138% FPL to 150% FPL","150% FPL to 200% FPL") &
               !(metal %in% c("Bronze","Catastrophic")) ~ 0,
             TRUE ~ NA_real_
           ))

## Decision support
data.clean <- data.clean %>%
  mutate(channel = 
           case_when(
             service_channel %in% c("CIA", "PBE") ~ "Insurance Agent",
             service_channel %in% c("SCR", "CEW", "CEC") ~ "Other Assistance",
             TRUE ~ as.character(service_channel)
           ))

data.pre2018 <- data.clean %>% filter(year<2018)

## DD data
data.dd <- data.clean %>%
  group_by(individual_id) %>%
  mutate(csr_post = (csr_eligible*(year>=2018) & subsidy_fpl_bracket %in% c("138% FPL or less","138% FPL to 150% FPL")),
         switch_plan = (plan_number!=previous_plan_number & !is.na(previous_plan_number))) %>%
  ungroup()



# Summary Statistics ------------------------------------------------------


## metal by type of assistance (all enrollees)
metal.assist.all <- data.pre2018 %>% group_by(channel, metal) %>%
  summarize(metal_count=n()) %>%
  mutate(metal_pct=metal_count/sum(metal_count),
         metal_pct=round(metal_pct,3))

## metal by type of assistance (new enrollees)
metal.assist.new <- data.pre2018 %>% filter(is.na(previous_plan_number)) %>%
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
stack.all <- ggplot(data.clean) +
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
enrollee.count.all <- data.pre2018 %>% count(year) %>%
  rename(all_count=n)
enrollee.count.new <- data.pre2018 %>% filter(is.na(previous_plan_number)) %>%
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


# Summary values for paper ------------------------------------------------

mean.dom.choice <- mean(data.pre2018$dominated_choice, na.rm=T)
tot.enroll <- sum(enrollee.count$all_count)
tot.enroll.new <- sum(enrollee.count$new_count)
first.year <- min(enrollee.count$year)
last.year <- max(enrollee.count$year)

# Initial Regressions -----------------------------------------------------

reg.data <- data.pre2018 %>%
  mutate(channel = as.factor(channel),
         gender = as.factor(Gender),
         language = as.factor(language),
         race = as.factor(race),
         region = as.factor(region),
         new_enrollee = is.na(previous_plan_number),
         any_assist = (channel=="Insurance Agent" | channel=="Other Assistance"),
         assist_agent = (channel=="Insurance Agent"),
         assist_other = (channel=="Other Assistance")) %>%
  select(channel, gender, AGE, language, race, region,
         premium21, subsidy, cheapest_premium, rating_area, FPL,
         dominated_choice, year, new_enrollee, household_id, 
         any_assist, assist_agent, assist_other)

reg1 <- felm(dominated_choice ~ any_assist + gender + AGE + language + premium21 +
       subsidy | region + year | 0 | region, 
     data=reg.data)

reg2 <- felm(dominated_choice ~ any_assist + gender + AGE + language + premium21 +
               subsidy | year + household_id | 0 | household_id, 
             data=reg.data)

reg3 <- felm(dominated_choice ~ assist_agent + assist_other + gender + AGE + language + premium21 +
               subsidy | year + household_id | 0 | household_id, 
             data=reg.data)


## DD estimates
reg.dd <- data.dd %>%
  mutate(channel = as.factor(channel),
         gender = as.factor(Gender),
         language = as.factor(language),
         race = as.factor(race),
         region = as.factor(region),
         new_enrollee = is.na(previous_plan_number),
         any_assist = (channel=="Insurance Agent" | channel=="Other Assistance"),
         assist_agent = (channel=="Insurance Agent"),
         assist_other = (channel=="Other Assistance")) %>%
  select(channel, gender, AGE, language, race, region,
         premium21, subsidy, cheapest_premium, rating_area, FPL, any_assist,
         dominated_choice, year, new_enrollee, household_id, csr_post, 
         csr_eligible, switch_plan, assist_agent, assist_other)


dd.reg1 <- felm(dominated_choice ~ any_assist + csr_post + any_assist*csr_post + gender + AGE + language + premium21 +
                  subsidy | region + year | 0 | region, 
                data= reg.dd )

dd.reg2 <- felm(dominated_choice ~ any_assist + csr_post + any_assist*csr_post + gender + AGE + language + premium21 +
                  subsidy | household_id + year | 0 | household_id, 
                data= reg.dd )


# Save workspace for knitr ------------------------------------------------

rm(list=c("data", "data.clean", "households", "zip3_choices",
          "plan_data", "product_definitions", "reg.data", "dd.data", "data.pre2018"))
save.image("data/R_workspace.Rdata")
