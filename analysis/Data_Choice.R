# Meta --------------------------------------------------------------------

## Date Created:  3/4/2020
## Date Edited:   3/9/2020
## Description:   Create dataset for choice model
## Notes:         This file requires that we've already created the household data
##                and the plan data.



# Clean data --------------------------------------------------------------

## Remove "flagged" households
hh.clean <- data.hh %>% filter(flagged==0)
data.clean <- data %>% filter(flagged==0)

## Oldest person per household
max.age <- data.ind %>%
  group_by(household_id, year) %>%
  summarize(oldest_member=max(AGE))

## Add some variables
hh.clean <- hh.clean %>%
  mutate(FPL_bracket = case_when(
    FPL <= 1.38 ~ "138orless",
    FPL > 1.38 & FPL <= 2.50 ~ "138to250",
    FPL > 2.50 & FPL <= 4 ~ "250to400",
    FPL > 4 ~ "400ormore")) %>%
  mutate(perc_18to34 = perc_18to25 + perc_26to34,
         perc_35to54 = perc_35to44 + perc_45to54) %>%
  left_join(max.age, by=c("household_id","year"))

  

## Predict who left market
hh.clean <- hh.clean %>%
  add_predictions(outside_logit, "pred_oom", type="response") %>%
  mutate(pred_oom = ifelse(is.na(plan_number_nocsr),pred_oom,NA),
         out_of_market = (pred_oom > mean(pred_oom, na.rm=TRUE))) %>%
  filter(out_of_market == FALSE | !is.na(plan_number_nocsr))
hh.clean <- as_tibble(hh.clean)
  

data.clean <- data.clean %>%
  left_join(hh.clean %>% select(household_id, year, out_of_market), 
            by=c("household_id","year")) %>%
  filter(out_of_market == FALSE | !is.na(plan_number_nocsr))
data.clean <- as_tibble(data.clean)


# Form choice sets --------------------------------------------------------
afford.threshold <- tibble(
  cutoff = c(0.08, 0.0805, 0.0813, 0.0816, 0.0805, 0.0830),
  year = c(2014, 2015, 2016, 2017, 2018, 2019)
)

zip3.choices <- zip3.choices %>%
  mutate(outside_option=1)
zip3 <- as_tibble(zip3.choices %>% distinct(zip3))

for (t in 2014:2019) {
  
  a.thresh <- as.numeric(afford.threshold %>% filter(year==t) %>% select(cutoff))
  
  for (i in 1:nrow(zip3))
  
    z <- as.numeric(zip3[i,1])
  
    choice.set <- zip3.choices %>% filter(Year==t, zip3==z) %>%
      pivot_longer(c(-zip3, -Region), names_to = "alt_plan_name", values_to = "offered") %>%
      filter(!is.na(offered)) %>%
      select(-offered)


    choice.data <- hh.clean %>% filter(year==t, zip3==z) %>%
      select(household_id, year, zip3, FPL, subsidized_members, 
             hh_plan_number_nocsr = plan_number_nocsr, hh_plan_id=plan_id, penalty,
             hh_plan_name = plan_name, oldest_member, cheapest_premium, subsidy, poverty_threshold) %>%
      full_join(choice.set, by=c("zip3")) %>%
      mutate(Issuer_Name = case_when(
              str_detect(alt_plan_name, "Anthem") ~ "Anthem",
              str_detect(alt_plan_name, "Blue_Shield") ~ "Blue_Shield",
              str_detect(alt_plan_name, "Health_Net") ~ "Health_Net",
              str_detect(alt_plan_name, "Kaiser") ~ "Kaiser",
              str_detect(alt_plan_name, "Molina") ~ "Molina",
              str_detect(alt_plan_name, "Sharp") ~ "SHARP",
              str_detect(alt_plan_name, "Chinese") ~ "Chinese_Community",
              str_detect(alt_plan_name, "Contra") ~ "Contra_Costa",
              str_detect(alt_plan_name, "LA_Care") ~ "LA_Care",
              str_detect(alt_plan_name, "Oscar") ~ "Oscar",
              str_detect(alt_plan_name, "United") ~ "United",
              str_detect(alt_plan_name, "Valley") ~ "Valley",
              str_detect(alt_plan_name, "Western") ~ "Western"),
            PLAN_NETWORK_TYPE = case_when(
              str_detect(alt_plan_name, "HMO") ~ "HMO",
              str_detect(alt_plan_name, "EPO") ~ "EPO",
              str_detect(alt_plan_name, "PPO") ~ "PPO",
              str_detect(alt_plan_name, "HSP") ~ "HSP"),
            MSP = case_when(
              str_detect(alt_plan_name, "MSP") ~ 1,
              TRUE ~ 0),
            Network_num = case_when(
              str_detect(alt_plan_name, "Sharp1") ~ 1,
              str_detect(alt_plan_name, "Sharp2") ~ 2,
              TRUE ~ 1
            ))
  
    ## Merge all possible choices
    full.choice.data <- choice.data %>%
      full_join(plan.data %>% mutate(Network_num=ifelse(is.na(Network_num),1,Network_num),
                                     PLAN_NETWORK_TYPE=as.character(PLAN_NETWORK_TYPE)) %>%
                  filter(ENROLLMENT_YEAR==t), 
                by=c("Issuer_Name","PLAN_NETWORK_TYPE","Network_num","MSP","Region"="region"))
    
    
    ## Remove plans that are not available for specific households.
    ## 1. ACS eligible households not eligible for "unenhanced silver plans"
    clean.choice.data <- full.choice.data %>%
      mutate(acs_eligible73 = ifelse(FPL>2 & FPL<=2.5 & subsidized_members>0,1,0),
             acs_eligible87 = ifelse(FPL>1.5 & FPL<=2 & subsidized_members>0,1,0),
             acs_eligible94 = ifelse(FPL<=1.5 & subsidized_members>0,1,0)) %>%
      filter( (metal_level=="Silver - Enhanced 73" & acs_eligible73==1) |
              (metal_level=="Silver - Enhanced 87" & acs_eligible87==1) |
              (metal_level=="Silver - Enhanced 94" & acs_eligible94==1) |
              metal!="Silver" | metal_level=="Silver")
    
    ## 2. Catastrophic plans only available for HH under age 30 without an affordable offer
    clean.choice.data <- clean.choice.data %>%
      mutate(
        eff_premium = case_when(
          subsidized_members>0 ~ (cheapest_premium-subsidy)*12,
          subsidized_members==0 ~ cheapest_premium*12),
        threshold = a.thresh*FPL*poverty_threshold)
    
    clean.choice.data <- clean.choice.data %>%
      filter((oldest_member<30 &  !is.na(oldest_member) & eff_premium>threshold & metal_level=="Minimum Coverage") | 
               metal_level!="Minimum Coverage")
  
    ## Identify plan choices and premiums
    clean.choice.data <- clean.choice.data %>%
      mutate(plan_choice=ifelse(hh_plan_name==Plan_Name,1,0)) %>%
      group_by(household_id) %>%
      mutate(insured=max(plan_choice)) %>% ungroup()
    
    clean.choice.data <- clean.choice.data %>%
      mutate(final_subsidy = ifelse(is.na(subsidy),0,subsidy),
             final_premium = case_when(
               insured==1 & metal_level!="Minimum Coverage" ~ pmax(Premium/1.278 - final_subsidy,0),
               insured==1 & metal_level=="Minimum Coverage" ~ Premium/1.278,
               insured==0 ~ penalty/12))
    
    ## Final dataset (for zip and year)
    choices <- clean.choice.data %>%
      select(household_id, year, zip3, Region, FPL, hh_plan_name, oldest_member,
             cheapest_premium, Issuer_Name, PLAN_NETWORK_TYPE, MSP, 
             Network_num, metal_level, Network_Breadth, Ownership, Deductible, Maximum_OOP,
             PCP_Copay, Specialist_Copay, Coinsurance, HSA, Star_Rating, Plan_Name,
             final_subsidy, final_premium, plan_choice, insured) %>%
      left_join(hh.clean %>% select(household_id, year, zip3, tax_unit_type,
                                    starts_with('lang_'), starts_with('perc_'),
                                    dominated_choice, service_channel, channel))
    
    assign(paste0('choices.',i,'.',t),)

    ## still need to collapse the small plans   
}


## check count of "in market" but uninsured households
insured.dat <- clean.choice.data %>% 
  group_by(household_id) %>%
  mutate(hh_count=row_number()) %>%
  filter(hh_count==1) %>%
  select(household_id, insured)


