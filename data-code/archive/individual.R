# Clean Individual Variables ----------------------------------------------

enroll.clean <- enroll.data %>%
  rename(year = enrlee_enrlmnt_yr,
         ind_id = indv_id_x)

## Income groups
enroll.clean <- enroll.clean %>% 
  mutate(FPL = subsidy_fpl_percent_int/100,
         missing_bracket = ifelse(subsidy_fpl_bracket 
                                  %in% c("FPL Unavailable",
                                         "Unsubsidized Applica"), 1, 0),
         subsidy_bracket = case_when(
           !is.na(FPL) & missing_bracket==1 & FPL >0 & FPL <= 1.38 ~ "138% FPL or less",
           !is.na(FPL) & missing_bracket==1 & FPL > 1.38 & FPL <= 1.50 ~ "138% FPL to 150% FPL",
           !is.na(FPL) & missing_bracket==1 & FPL > 1.50 & FPL <= 2.00 ~ "150% FPL to 200% FPL",
           !is.na(FPL) & missing_bracket==1 & FPL > 2.00 & FPL <= 2.50 ~ "200% FPL to 250% FPL",
           !is.na(FPL) & missing_bracket==1 & FPL > 2.50 & FPL <= 4.00 ~ "250% FPL to 400% FPL",
           !is.na(FPL) & missing_bracket==1 & FPL > 4.00 ~ "400% FPL or greater",
           missing_bracket==1 & metal_level_enhanced == "Silver - Enhanced 73" ~ "250% FPL to 400% FPL",
           missing_bracket==1 & metal_level_enhanced == "Silver - Enhanced 87" ~ "150% FPL to 200% FPL",
           missing_bracket==1 & metal_level_enhanced == "Silver - Enhanced 94" ~ "150% FPL or less",
           missing_bracket==1 & subsidy_eligible != "Subsidy Eligible" & 
             !is.na(net_premium_amt_int) & gross_premium_amt_int==net_premium_amt_int ~ "400% FPL or less",
           missing_bracket==1 & 
             !is.na(gross_premium_amt_int) & gross_premium_amt_int>net_premium_amt_int ~ "400% FPL or less",
           missing_bracket==1 & subsidy_eligible == "Subsidy Eligible" ~ "400% FPL or less",
           subsidy_eligible == "Subsidy Eligible" & subsidy_fpl_bracket == "400% FPL or greater" &
             !is.na(net_premium_amt_int) & gross_premium_amt_int>=net_premium_amt_int ~ "400% FPL or less",
           missing_bracket==1 & subsidy_eligible != "Subsidy Eligible" ~ "400% FPL or greater",
           missing_bracket==0 ~ subsidy_fpl_bracket,
           TRUE ~ NA_character_)
  )

## Metal level
enroll.clean <- enroll.clean %>%
  mutate(metal_level = case_when(
    is.na(metal_level_enhanced) & str_detect(plan_name, "Bronze") ~ "Bronze",
    is.na(metal_level_enhanced) & str_detect(plan_name, "Silver") ~ "Silver",
    is.na(metal_level_enhanced) & str_detect(plan_name, "73") ~ "Silver - Enhanced 73",
    is.na(metal_level_enhanced) & str_detect(plan_name, "87") ~ "Silver - Enhanced 87",
    is.na(metal_level_enhanced) & str_detect(plan_name, "94") ~ "Silver - Enhanced 94",
    is.na(metal_level_enhanced) & str_detect(plan_name, "Gold") ~ "Gold",
    is.na(metal_level_enhanced) & str_detect(plan_name, "Platinum") ~ "Platinum",
    is.na(metal_level_enhanced) & str_detect(plan_name, "Minimum Coverage") ~ "Minimum Coverage",
    !is.na(metal_level_enhanced) ~ metal_level_enhanced,
    TRUE ~ "Silver - Enhanced 87")
  ) %>%
  mutate(metal_level = case_when(
    metal_level %in% c("Silver - Enhanced 87", "Silver - Enhanced 94") & 
      subsidy_bracket=="200% FPL to 250% FPL" ~ "Silver - Enhanced 73",
    metal_level %in% c("Silver - Enhanced 73", "Silver - Enhanced 94") & 
      subsidy_bracket=="150% FPL to 200% FPL" ~ "Silver - Enhanced 87",
    metal_level %in% c("Silver - Enhanced 87", "Silver - Enhanced 73") & 
      subsidy_bracket %in% c("138% FPL or less", "138% FPL to 150% FPL",
                             "150% FPL or less") ~ "Silver - Enhanced 94",    
    metal_level %in% c("Silver - Enhanced 87", "Silver - Enhanced 73", "Silver - Enhanced 94") & 
      !subsidy_bracket %in% c("138% FPL or less", "138% FPL to 150% FPL",
                             "150% FPL or less", "150% FPL to 200% FPL",
                             "200% FPL to 250% FPL") ~ "Silver",
    TRUE ~ metal_level)
  )


## Gender
enroll.clean <- enroll.clean %>%
  mutate(gender = case_when(
           gender=="Male" ~ 1,
           gender=="Female" ~ 0,
           TRUE ~ NA_real_)
         )


## Condensed metal
enroll.clean <- enroll.clean %>%
  mutate(metal = case_when(
    metal_level == "Minimum Coverage" ~ "Minimum Coverage",
    metal_level == "Bronze" ~ "Bronze",
    metal_level %in% c("Silver",
                       "Silver - Enhanced 73",
                       "Silver - Enhanced 87",
                       "Silver - Enhanced 94") ~ "Silver",
    metal_level == "Gold" ~ "Gold",
    metal_level == "Platinum" ~ "Platinum",
    TRUE ~ NA_character_)
  )

## Insurer
enroll.clean <- enroll.clean %>%
  mutate(insurer = case_when(
    str_detect(issuer_name, "Anthem") ~ "Anthem",
    str_detect(issuer_name, "Blue Shield") ~ "BlueShield",
    str_detect(issuer_name, "Chinese") ~ "ChineseCommunity",
    str_detect(issuer_name, "Contra Costa") ~ "ContraCosta",
    str_detect(issuer_name, "Health Net") ~ "HealthNet",
    str_detect(issuer_name, "LA Care") ~ "LACare",
    str_detect(issuer_name, "Kaiser") ~ "Kaiser",
    str_detect(issuer_name, "Molina") ~ "Molina",
    str_detect(issuer_name, "SHARP") ~ "Sharp",
    str_detect(issuer_name, "Oscar") ~ "Oscar",
    str_detect(issuer_name, "UnitedHealthcare") ~ "United",
    str_detect(issuer_name, "Valley") ~ "Valley",
    str_detect(issuer_name, "Western") ~ "Western")
  )

## Open Enrollment
enroll.clean <- enroll.clean %>%
  mutate(start_week = as.numeric(str_sub(cov_start_dt_WK, 6, 7)),
         end_week = as.numeric(str_sub(cov_end_dt_WK, 6, 7)),
         oep = case_when(
           year==2014 & start_week<=17 ~ 1,
           year %in% c(2015, 2016, 2017) & start_week<=9 ~ 1,
           year==2018 & start_week<=8 ~ 1,
           year==2019 & start_week<=4 ~ 1,
           TRUE ~ 0)
  )

## Race
enroll.clean <- enroll.clean %>%
  mutate(race = case_when(
    race_ethnicity %in% c("American Indian",
                          "Native Hawaiian",
                          "Other",
                          "Multiple Races") ~ "Other",
    race_ethnicity == "Latino" ~ "Hispanic",
    race_ethnicity == "Asian" ~ "Asian",
    race_ethnicity == "White" ~ "White",
    race_ethnicity == "Black or Africa" ~ "Black",
    TRUE ~ "Unspecified")
  )


## Age group
enroll.clean <- enroll.clean %>%
  mutate(age_group=case_when(
    age < 18 ~ "0to17",
    age >= 18 & age < 26 ~ "18to25",
    age >= 26 & age < 35 ~ "26to34",
    age >= 35 & age < 45 ~ "35to44",
    age >= 45 & age < 55 ~ "45to54",
    age >= 55 & age < 65 ~ "55to64",
    age >= 65 & age < 120 ~ "65plus",
    TRUE ~ NA_character_)
  )


## Language
enroll.clean <- enroll.clean %>%
  mutate(language = case_when(
    language_spoken == "English" ~ "English",
    language_spoken == "Spanish" ~ "Spanish",
    !language_spoken %in% c("(nonres","English","Spanish") ~ "Other Language",
    TRUE ~ NA_character_)
  )



## Decision support
enroll.clean <- enroll.clean %>%
  mutate(channel = 
           case_when(
             service_channel %in% c("CIA", "PBE") ~ "Insurance Agent",
             service_channel %in% c("SCR", "CEW", "CEC") ~ "Other Assistance",
             TRUE ~ as.character(service_channel)
           ))




# Clean region data -------------------------------------------------------

enroll.clean <- enroll.clean %>%
  mutate(hios_id_14 = str_sub(hios_id_16,1,14),
         region = case_when(
           insurer=="Sharp" ~ 19,
           insurer=="Valley" ~ 7,
           is.na(region) & insurer=="ChineseCommunity" & zip3 %in% c(940, 944) ~ 8,
           is.na(region) & insurer=="ChineseCommunity" & zip3==941 ~ 4,
           is.na(region) & insurer=="Western" & 
             hios_id_14 %in% c("93689CA0110001","93689CA0120001",
                               "93689CA0110002","93689CA0120004",
                               "93689CA0130002","93689CA0120005") ~ 2,
           is.na(region) & insurer=="Western" & 
             hios_id_14 %in% c("93689CA0150001","93689CA0160001",
                               "93689CA0150002","93689CA0160002",
                               "93689CA0170001","93689CA0160003") ~ 3,
           is.na(region) & insurer=="Molina" & zip3==922 ~ 13,
           is.na(region) & insurer=="Oscar" & zip3==906 ~ 18,
           is.na(region) & insurer=="Anthem" & zip3 %in% c(959,960,954,955) ~ 1,
           is.na(region) & insurer=="Anthem" & zip3 %in% c(932,952,953) ~ 10,
           is.na(region) & insurer=="Anthem" & zip3 %in% c(940,950,951) ~ 7,
           is.na(region) & insurer=="Kaiser" & zip3==954 ~ 2,
           is.na(region) & insurer=="Kaiser" & zip3==922 ~ 17,
           is.na(region) & insurer=="Kaiser" & zip3==935 ~ 15,
           is.na(region) & insurer=="HealthNet" & zip3 %in% c(907,935) ~ 15,
           is.na(region) & insurer=="HealthNet" & zip3==913 ~ 16,
           is.na(region) & insurer=="HealthNet" & zip3==922 ~ 17,
           !is.na(region) ~ region,
           TRUE ~ NA_real_)
         )
  
enroll.clean <- enroll.clean %>%
  left_join(zip3.choices %>% ungroup() %>%
              distinct(zip3, region) %>% 
              mutate(zip_region_match=1), by=c("zip3", "region"))



# Final enrollment data ---------------------------------------------------

## Remove antiquated variables
enroll.clean <- enroll.clean %>%
  select(-c(X1, metal_level_enhanced, race_ethnicity, 
            subsidy_fpl_percent_int, subsidy_fpl_bracket,
            missing_bracket, subsidy_eligible,
            language_spoken, service_channel))


# Drop individuals with multiple plans in the same year
enroll.unique <- enroll.clean %>%
  add_count(ind_id, year) %>%
  filter(n==1) %>% ungroup() %>% select(-n)

enroll.dup <- enroll.clean %>%
  add_count(ind_id, year) %>%
  filter(n>1) %>% ungroup() %>% select(-n) 

enroll.dup.clean <- enroll.dup %>%
  mutate(select_week=substr(ENRLEE_PLAN_SELECT_DT_WK, 6, 7),
         select_week=as.numeric(select_week)) %>%
  group_by(ind_id, year) %>%
  mutate(early_week=min(select_week)) %>%
  filter(early_week==select_week) %>%
  arrange(ind_id, year, early_week, net_premium_amt_int) %>%
  add_count(ind_id, year) %>%
  filter(n==1) %>% select(-c(n, select_week, early_week))
  

## Combine unique observations
enroll.final <- bind_rows(enroll.unique, enroll.dup.clean)
  

## Flag questionable observations based on:
enroll.final <- enroll.final %>%
  mutate(missing_flag = case_when (
    is.na(gender) ~ 1,
    is.na(age) ~ 1,
    !is.na(age) & age>120 ~ 1,
    is.na(zip3) ~ 1,
    is.na(region) ~ 1,
    is.na(issuer_name) ~ 1,
    is.na(metal_level) ~ 1,
    TRUE ~ 0
  ))