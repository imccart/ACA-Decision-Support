# Meta --------------------------------------------------------------------

## Title:         Decision Assistance and Health Insurance Choice
## Author:        Ian McCarthy & Evan Saltzman
## Date Created:  10/11/2019
## Date Edited:   11/5/2019
## Description:   Clean plan-level dataset


# Load data ---------------------------------------------------------------

plan.data <- read.csv("data/ca_plan_data2.csv") # Covered California plan data


# Clean Plan Data ---------------------------------------------------------

## rename insurers
plan.data <- plan.data %>%
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
plan.data <- plan.data %>%
  mutate(metal = as.character(metal_level),
         metal = replace(metal, metal %in% c("Silver - Enhanced 73",
                                             "Silver - Enhanced 87",
                                             "Silver - Enhanced 94"), 
                         "Silver") )

