# Meta --------------------------------------------------------------------

## Date Created:  10/11/2019
## Date Edited:   9/24/2020
## Description:   Clean plan-level dataset


# Clean Plan Data ---------------------------------------------------------

## rename insurers
plan.data <- plan.data %>%
  mutate(
    insurer = case_when(
      Issuer_Name == "Anthem Blue Cross" ~ "Anthem",
      Issuer_Name == "Blue Shield" ~ "BlueShield",
      Issuer_Name == "Chinese Community" ~ "ChineseCommunity",
      Issuer_Name == "Contra Costa Health Plan" ~ "ContraCosta",
      Issuer_Name == "Health Net" ~ "HealthNet",
      Issuer_Name == "LA Care" ~ "LACare",
      Issuer_Name == "UnitedHealthcare" ~ "United",
      Issuer_Name == "Western Health" ~ "Western",
      Issuer_Name == "Sharp" ~ "Sharp",
      Issuer_Name == "Kaiser" ~ "Kaiser",
      Issuer_Name == "Oscar" ~ "Oscar",
      Issuer_Name == "Molina" ~ "Molina",
      Issuer_Name == "Valley" ~ "Valley",
      TRUE ~ Issuer_Name
    )
  )


## Metal tier
plan.data <- plan.data %>%
  mutate(metal = as.character(metal_level),
         metal = replace(metal, metal %in% c("Silver - Enhanced 73",
                                             "Silver - Enhanced 87",
                                             "Silver - Enhanced 94"), 
                         "Silver") )

