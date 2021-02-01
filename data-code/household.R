# Meta --------------------------------------------------------------------

## Date Created:  10/11/2019
## Date Edited:   3/4/2020
## Description:   Clean household-level dataset


# Clean Household Data ---------------------------------------------------


## Plan characteristics
data.hh <- households %>% 
  separate(zip_region_year, c(NA,"region",NA), sep="_") %>%
  mutate(region = as.integer(region)) %>%
  left_join( (plan.data %>% 
                select(plan_name=Plan_Name2, region, 
                       year=ENROLLMENT_YEAR, 
                       plan_network_type=PLAN_NETWORK_TYPE,
                       metal, 
                       insurer=Issuer_Name) %>%
                mutate(plan_name=as.character(plan_name))),
             by=c("plan_name","region","year")) %>%
  mutate(plan_network_type=as.character(plan_network_type)) %>%
  mutate( metal = replace(metal, metal == "Minimum Coverage", "Catastrophic"))


## Income groups
data.hh <- data.hh %>%
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
hh.language <- data.ind %>% 
  mutate(lang_english = (language == "English"),
         lang_spanish = (language == "Spanish"),
         lang_other = (language == "Other")) %>%
  group_by(household_id, year) %>%
  summarize(lang_english=max(lang_english),
            lang_spanish=max(lang_spanish),
            lang_other=max(lang_other))

data.hh <- data.hh %>%
  left_join(hh.language, by=c("household_id", "year"))


## Subsidies
data.hh <- data.hh %>%
  mutate(subsidy_eligible = as.numeric(subsidized_members>0),
         subsidized = case_when(
           subsidy_eligible == 1 ~ "Subsidized",
           subsidy_eligible == 0 ~ "Unsubsidized",
           TRUE ~ NA_character_
         ),
         csr_eligible = as.numeric(subsidized == "Subsidized" & FPL <= 2.50)
  )


## Dominated Plans
data.hh <- data.hh %>%
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
data.hh <- data.hh %>%
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
data.hh <- data.hh %>%
  left_join( (data.ind %>% 
                group_by(household_id, year) %>% 
                summarize(service_channel=first(service_channel))),
              by=c("household_id","year")) %>%
  mutate(channel = 
           case_when(
             service_channel %in% c("CIA", "PBE") ~ "Insurance Agent",
             service_channel %in% c("SCR", "CEW", "CEC") ~ "Other Assistance",
             TRUE ~ as.character(service_channel)
           ))


