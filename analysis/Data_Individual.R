# Meta --------------------------------------------------------------------

## Date Created:  10/11/2019
## Date Edited:   3/4/2020
## Description:   Clean individual-level dataset


# Clean Individual Data ---------------------------------------------------

## Drop all uninsured records for this analysis
data.ind <- data %>%
  filter(!is.na(plan_id))

## Create Age Group Variable
data.ind <- data.ind %>%
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
data.ind <- data.ind %>%
  mutate(
    Gender = case_when(
      gender == 1 ~ "Male",
      gender == 0 ~ "Female"
    )
  )

## Metal
data.ind <- data.ind %>%
  mutate(metal = as.character(metal_level_enhanced),
         metal = replace(metal, metal %in% c("Silver - Enhanced 73",
                                             "Silver - Enhanced 87",
                                             "Silver - Enhanced 94"), 
                         "Silver"),
         metal = replace(metal, metal == "Minimum Coverage", "Catastrophic"))


## Income groups
data.ind <- data.ind %>%
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
data.ind <- data.ind %>%
  mutate(
    language = case_when(
      language_spoken == "English" ~ "English",
      language_spoken == "Spanish" ~ "Spanish",
      !(language_spoken %in% c("(nonres","English","Spanish")) ~ "Other Language",
      TRUE ~ NA_character_
    )
  )



## Decision support
data.ind <- data.ind %>%
  mutate(channel = 
           case_when(
             service_channel %in% c("CIA", "PBE") ~ "Insurance Agent",
             service_channel %in% c("SCR", "CEW", "CEC") ~ "Other Assistance",
             TRUE ~ as.character(service_channel)
           ))


