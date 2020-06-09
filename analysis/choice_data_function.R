# Meta --------------------------------------------------------------------

## Date Created:  5/12/2020
## Date Edited:   5/25/2020
## Description:   Function to create choice-level datasets and save subset for future use


# Estimation function -----------------------------------------------------

choice.data.fnc <- function(t, r) {
  
  a.thresh <- as.numeric(afford.threshold %>% filter(year==t) %>% select(cutoff))
  outside=tibble(
    plan_name="Uninsured",
    region=r,
    Issuer_Name="Outside_Option"
  )
  
  choice.set <- plan.data %>% filter(ENROLLMENT_YEAR==t, region==r) %>%
    select(plan_name=Plan_Name2, Issuer_Name, region, PLAN_NETWORK_TYPE, metal_level, metal, 
           Premium, MSP, HSA) %>%
    group_by(plan_name, region) %>%
    summarize(Issuer_Name=first(Issuer_Name),
              PLAN_NETWORK_TYPE=first(PLAN_NETWORK_TYPE),
              metal_level=first(metal_level),
              metal=first(metal),
              Premium=mean(Premium, na.rm=TRUE),
              MSP=min(MSP, na.rm=TRUE),
              HSA=min(HSA, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(plan_name=as.character(plan_name)) %>%
    bind_rows(outside)
  
  
  full.choice.data <- hh.clean %>% filter(year==t, region==r) %>%
    select(household_id, year, zip3, FPL, subsidized_members, hh_region=region, hh_rating=rating_factor,
           hh_plan_number = plan_number_nocsr, hh_plan_id=plan_id, penalty, hh_insurer=insurer,
           hh_plan_name = plan_name, oldest_member, cheapest_premium, subsidy, poverty_threshold) %>%
    full_join(choice.set, by=c("hh_region"="region")) 
  
  
  ## Remove plans that are not available for specific households.
  ## 1. ACS eligible households not eligible for "unenhanced silver plans"
  clean.choice.data <- full.choice.data %>%
    mutate(acs_eligible73 = ifelse(FPL>2 & FPL<=2.5 & subsidized_members>0,1,0),
           acs_eligible87 = ifelse(FPL>1.5 & FPL<=2 & subsidized_members>0,1,0),
           acs_eligible94 = ifelse(FPL<=1.5 & subsidized_members>0,1,0),
           HMO=case_when(
             PLAN_NETWORK_TYPE=="HMO" ~ 1,
             is.na(PLAN_NETWORK_TYPE) ~ 0)) %>%
    filter( (metal_level=="Silver - Enhanced 73" & acs_eligible73==1) |
              (metal_level=="Silver - Enhanced 87" & acs_eligible87==1) |
              (metal_level=="Silver - Enhanced 94" & acs_eligible94==1) |
              metal!="Silver" | 
              (metal_level=="Silver" & acs_eligible73==0 & acs_eligible87==0 & acs_eligible94==0 ) |
              is.na(metal))
  
  ## 2. Catastrophic plans only available for HH under age 30 without an affordable offer
  clean.choice.data <- clean.choice.data %>%
    mutate(
      eff_premium = case_when(
        subsidized_members>0 ~ (cheapest_premium-subsidy)*12,
        subsidized_members==0 ~ cheapest_premium*12),
      threshold = a.thresh*FPL*poverty_threshold)
  
  clean.choice.data <- clean.choice.data %>%
    filter((oldest_member<30 &  !is.na(oldest_member) & eff_premium>threshold & metal_level=="Minimum Coverage") | 
             metal_level!="Minimum Coverage" | is.na(metal))
  
  ## Identify plan choices, premiums, and form other variables
  clean.choice.data <- clean.choice.data %>%
    mutate(plan_choice = case_when(
      hh_plan_name==plan_name & !is.na(hh_plan_name) & !is.na(plan_name) & !is.na(hh_plan_number) ~ 1,
      TRUE ~ 0)) %>%
    group_by(household_id) %>%
    mutate(insured=max(plan_choice)) %>% ungroup()
  
  clean.choice.data <- clean.choice.data %>%
    mutate(final_subsidy = ifelse(is.na(subsidy),0,subsidy),
           hh_premium = (Premium/1.278)*hh_rating,
           final_premium = case_when(
             metal_level!="Minimum Coverage" | (is.na(metal) & Issuer_Name!="Outside_Option") ~ pmax(hh_premium - final_subsidy,0),
             metal_level=="Minimum Coverage" ~ hh_premium,
             Issuer_Name=="Outside_Option" ~ penalty/12),
           platinum=ifelse(metal=="Platinum",1,0),
           gold=ifelse(metal=="Gold",1,0),
           silver=ifelse(metal=="Silver",1,0),
           bronze=ifelse(metal=="Bronze",1,0),
           AV=case_when(
             metal_level=="Minimum Coverage" ~ 0.55,
             metal_level=="Bronze" ~ 0.6,
             metal_level=="Silver" ~ 0.7,
             metal_level=="Gold" ~ 0.8,
             metal_level=="Platinum" ~ 0.9,
             metal_level=="Silver - Enhanced 73" ~ 0.73,
             metal_level=="Silver - Enhanced 87" ~ 0.87,
             metal_level=="Silver - Enhanced 94" ~ 0.94,
             is.na(metal_level) & Issuer_Name=="Outside_Option" ~ 0))
  
  ## Collapse small insurers into one
  clean.choice.large <- clean.choice.data %>%
    filter(Issuer_Name %in% c('Anthem', 'Blue_Shield', 'Kaiser', 'Health_Net', 'Outside_Option')) %>%
    mutate(monthly_penalty=penalty/12) %>%
    select(household_id, year, FPL, hh_plan_name, oldest_member, hh_region, hh_insurer,
           cheapest_premium, Issuer_Name, PLAN_NETWORK_TYPE, MSP, HMO, HSA,
           metal_level, metal, plan_name,
           final_subsidy, final_premium, plan_choice, insured, monthly_penalty,
           platinum, gold, silver, bronze, AV)
  
  clean.choice.small <- clean.choice.data %>%
    filter(!Issuer_Name %in% c('Anthem', 'Blue_Shield', 'Kaiser', 'Health_Net', 'Outside_Option')) %>%
    group_by(household_id, year, hh_region, metal) %>%
    summarize(final_premium=min(final_premium, na.rm=TRUE),
              plan_choice=max(plan_choice,na.rm=TRUE),
              FPL=first(FPL), hh_plan_name=first(hh_plan_name),
              oldest_member=first(oldest_member),
              cheapest_premium=first(cheapest_premium), insured=first(insured),
              penalty=first(penalty),
              HMO=mean(HMO,na.rm=TRUE), HSA=mean(HSA, na.rm=TRUE),
              platinum=mean(platinum, na.rm=TRUE), gold=mean(gold, na.rm=TRUE),
              silver=mean(silver, na.rm=TRUE), bronze=mean(bronze, na.rm=TRUE),
              AV=mean(AV, na.rm=TRUE))%>%
    mutate(monthly_penalty=penalty/12) %>%      
    select(household_id, year, FPL, hh_plan_name, oldest_member, hh_region,
           cheapest_premium, plan_choice, insured, final_premium, monthly_penalty,
           platinum, gold, silver, bronze, AV, metal) %>%
    mutate(Issuer_Name='Small_Insurer', hh_insurer="Small_Insurer",
           plan_name=case_when(
             platinum==1~ "Small_P",
             gold==1~ "Small_G",
             silver==1~ "Small_SIL",
             bronze==1~ "Small_BR",
             metal=="Minimum Coverage" ~ "Small_CAT")
    )
  
  
  ## Final dataset (for zip and year)
  choices <- bind_rows(clean.choice.large,clean.choice.small) %>%
    select(household_id, year, hh_region, FPL, hh_plan_name, hh_insurer, oldest_member,
           cheapest_premium, Issuer_Name, PLAN_NETWORK_TYPE, MSP, metal, HMO, HSA,
           metal_level, final_subsidy, final_premium, plan_choice, insured, monthly_penalty,
           platinum, silver, gold, bronze, AV, plan_name) %>%
    left_join(hh.clean %>% select(household_id, year, hh_region=region, tax_unit_type, hh_size=household_size,
                                  starts_with('lang_'), starts_with('perc_'), starts_with('exempt'),
                                  dominated_choice, service_channel, channel),
              by=c("household_id", "year", "hh_region")) %>%
    mutate(metal=ifelse(is.na(metal),"Other",metal),
           plan_choice = case_when(
             plan_choice==1 & insured==1 ~ 1,
             plan_choice==0 & insured==1 ~ 0,
             plan_choice==0 & insured==0 & plan_name=="Uninsured" ~ 1,
             plan_choice==0 & insured==0 & plan_name!="Uninsured" ~ 0))
  
  
  # Create smaller dataset with only relevant variables and consistent choice set
  small.dat <- choices %>%
    filter(!is.na(final_premium), !is.na(plan_name)) %>%
    mutate(net_premium=final_premium-monthly_penalty,
           net_premium=net_premium/hh_size,
           FPL_250to400=ifelse(FPL > 2.50 & FPL <= 4.00, 1, 0),
           FPL_400plus=ifelse(FPL >  4.00, 1, 0),
           HMO=ifelse(PLAN_NETWORK_TYPE=="HMO" & !is.na(PLAN_NETWORK_TYPE),1,0),
           any_0to17=ifelse(perc_0to17>0,1,0),
           any_18to34=ifelse(perc_18to34>0,1,0),
           any_35to54=ifelse(perc_35to54>0,1,0),
           any_black=ifelse(perc_black>0,1,0),
           any_hispanic=ifelse(perc_hispanic>0,1,0),
           any_asian=ifelse(perc_asian>0,1,0),
           any_other=ifelse(perc_other>0,1,0),
           any_male=ifelse(perc_male>0,1,0),
           uninsured_plan=ifelse(plan_name=="Uninsured",1,0),) %>%
    mutate_at(vars("any_0to17", "any_18to34", "any_35to54",
                   "any_black", "any_hispanic", "any_asian",
                   "any_other", "any_male", "FPL_250to400",
                   "FPL_400plus", "hh_size", "HSA"),
              list(prem = ~.*net_premium)) %>%
    mutate(Anthem=ifelse(Issuer_Name=="Anthem",1,0),
           Blue_Shield=ifelse(Issuer_Name=="Blue_Shield",1,0),
           Kaiser=ifelse(Issuer_Name=="Kaiser",1,0),
           Health_Net=ifelse(Issuer_Name=="Health_Net",1,0),
           any_0to17=any_0to17*(1-uninsured_plan),
           any_black=any_black*(1-uninsured_plan),
           any_hispanic=any_hispanic*(1-uninsured_plan),
           FPL_250to400=FPL_250to400*(1-uninsured_plan),
           FPL_400plus=FPL_400plus*(1-uninsured_plan),
           hh_size=hh_size*(1-uninsured_plan)) %>%
    mutate_at(vars(HMO, HSA, platinum, gold, silver, bronze), ~replace(., is.na(.), 0)) %>%
    group_by(household_id) %>%
    mutate(plan_name=str_replace(plan_name, "SIL.*","SIL")) %>%
    arrange(household_id, plan_name) %>%
    ungroup()
  
  
  # Estimation and prediction samples
  untreated.dat <- small.dat %>%
    filter(channel=="Unassisted") %>% 
    select(choice=plan_choice, plan_name, premium=net_premium, household_number=household_id,
           uninsured_plan, AV, hh_size, any_0to17, any_black, any_hispanic, FPL_250to400, FPL_400plus, 
           HMO, HSA, platinum, gold, silver, bronze,
           hh_size_prem, any_0to17_prem, any_black_prem, any_hispanic_prem, FPL_250to400_prem, FPL_400plus_prem, 
           Anthem, Blue_Shield, Kaiser, Health_Net)
  
  treated.dat <- small.dat %>%
    filter(channel!="Unassisted") %>% 
    select(choice=plan_choice, plan_name, premium=net_premium, household_number=household_id,
           uninsured_plan, AV, hh_size, any_0to17, any_black, any_hispanic, FPL_250to400, FPL_400plus, 
           HMO, HSA, platinum, gold, silver, bronze,
           hh_size_prem, any_0to17_prem, any_black_prem, any_hispanic_prem, FPL_250to400_prem, FPL_400plus_prem, 
           Anthem, Blue_Shield, Kaiser, Health_Net)
  
  unique.hh <- untreated.dat %>%
    group_by(household_number) %>%
    mutate(hh_count=seq(n())) %>%
    filter(hh_count==1) %>%
    select(household_number) %>% ungroup()
  sample.hh <- sample_frac(unique.hh, size=0.05, replace=FALSE)
  
  unique.hh.oos <- treated.dat %>%
    group_by(household_number) %>%
    mutate(hh_count=seq(n())) %>%
    filter(hh_count==1) %>%
    select(household_number) %>% ungroup()
  sample.hh.oos <- sample_frac(unique.hh.oos, size=0.2, replace=FALSE)
  
    
  ## Final data to Julia
  julia.data <- untreated.dat %>%
    inner_join(sample.hh, by=c("household_number")) %>%          
    select(plan_name, household_number, choice, premium,
           platinum, gold, silver, bronze, HSA, HMO, AV, uninsured_plan,
           hh_size_prem, any_0to17_prem, FPL_250to400_prem, FPL_400plus_prem, any_black_prem, any_hispanic_prem,
           hh_size, any_0to17, FPL_250to400, FPL_400plus, any_black, any_hispanic,
           Anthem, Blue_Shield, Kaiser, Health_Net)
  
  julia.oos <- treated.dat %>%
    inner_join(sample.hh.oos, by=c("household_number")) %>%              
    select(plan_name, household_number, choice, premium,
           platinum, gold, silver, bronze, HSA, HMO, AV, uninsured_plan, 
           hh_size_prem, any_0to17_prem, FPL_250to400_prem, FPL_400plus_prem, any_black_prem, any_hispanic_prem,
           hh_size, any_0to17, FPL_250to400, FPL_400plus, any_black, any_hispanic,
           Anthem, Blue_Shield, Kaiser, Health_Net)
  
  return(list("julia.data"=julia.data, "julia.oos"=julia.oos))
}