# Meta --------------------------------------------------------------------

## Date Created:  3/4/2020
## Date Edited:   3/6/2020
## Description:   Create dataset for choice model
## Notes:         This file requires that we've already created the household data
##                and the plan data.



# Clean data --------------------------------------------------------------

## Remove "flagged" households
hh.clean <- data.hh %>% filter(flagged==0)
data.clean <- data %>% filter(flagged==0)


## Add some variables
hh.clean <- hh.clean %>%
  mutate(FPL_bracket = case_when(
    FPL <= 1.38 ~ "138orless",
    FPL > 1.38 & FPL <= 2.50 ~ "138to250",
    FPL > 2.50 & FPL <= 4 ~ "250to400",
    FPL > 4 ~ "400ormore")) %>%
  mutate(perc_18to34 = perc_18to25 + perc_26to34,
         perc_35to54 = perc_35to44 + perc_45to54)


## Predict who left market
hh.clean <- hh.clean %>%
  add_predictions(outside_logit, "pred_oom", type="response") %>%
  mutate(pred_oom = ifelse(is.na(plan_number_nocsr),pred_oom,NA),
         out_of_market = (pred_oom > mean(pred_oom, na.rm=TRUE))) %>%
  filter(out_of_market == FALSE)
hh.clean <- as_tibble(hh.clean)
  

data.clean <- data.clean %>%
  left_join(hh.clean %>% select(household_id, year, out_of_market), 
            by=c("household_id","year")) %>%
  filter(out_of_market == FALSE)
data.clean <- as_tibble(data.clean)


# Form choice sets --------------------------------------------------------

choice.set <- zip3.choices %>%
  pivot_longer(c(-Year, -zip3, -Region), names_to = "plan_name", values_to = "offered") %>%
  filter(!is.na(offered)) %>%
  select(-offered)


choice.data <- hh.clean %>% select(household_id, year, zip3, FPL, subsidized_members) %>%
  full_join(choice.set, by=c("year"= "Year","zip3")) %>%
  mutate(Issuer_Name = case_when(
          str_detect(plan_name, "Anthem") ~ "Anthem",
          str_detect(plan_name, "Blue_Shield") ~ "Blue_Shield",
          str_detect(plan_name, "Health_Net") ~ "Health_Net",
          str_detect(plan_name, "Kaiser") ~ "Kaiser",
          str_detect(plan_name, "Molina") ~ "Molina",
          str_detect(plan_name, "Sharp") ~ "SHARP",
          str_detect(plan_name, "Chinese") ~ "Chinese_Community",
          str_detect(plan_name, "Contra") ~ "Contra_Costa",
          str_detect(plan_name, "LA_Care") ~ "LA_Care",
          str_detect(plan_name, "Oscar") ~ "Oscar",
          str_detect(plan_name, "United") ~ "United",
          str_detect(plan_name, "Valley") ~ "Valley",
          str_detect(plan_name, "Western") ~ "Western"),
         PLAN_NETWORK_TYPE = case_when(
          str_detect(plan_name, "HMO") ~ "HMO",
          str_detect(plan_name, "EPO") ~ "EPO",
          str_detect(plan_name, "PPO") ~ "PPO",
          str_detect(plan_name, "HSP") ~ "HSP"),
         MSP = case_when(
           str_detect(plan_name, "MSP") ~ 1,
           TRUE ~ 0),
         Network_num = case_when(
           str_detect(plan_name, "Sharp1") ~ 1,
           str_detect(plan_name, "Sharp2") ~ 2,
           TRUE ~ 1
         ))

afford.threshold <- tibble(
  cutoff = c(0.08, 0.0805, 0.0813, 0.0816, 0.0805, 0.0830),
  year = c(2014, 2015, 2016, 2017, 2018, 2019)
)
  
  <- c(0.08,0.0805,0.0813,0.0816,0.0805,0.830)
names(affordability_threshold) <- c("2014","2015","2016","2017","2018","2019")


for (t in 2014:2019) {

  ## Merge all possible choices
  full.choice.data <- choice.data %>% filter(year==t) %>%
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
    mutate(silver_option = case_when(
      acs_eligible73==1 & metal_level=="Silver - Enhanced 73" ~ 1,
      acs_eligible87==1 & metal_level=="Silver - Enhanced 87" ~ 1,
      acs_eligible94==1 & metal_level=="Silver - Enhanced 94" ~ 1)
    ) %>%
    filter( (metal=="Silver" & silver_option==1) | metal!="Silver")
  
  ## 2. Catastrophic plans only available for HH under age 30 without an affordable offer
  clean.choice.data <- clean.choice.data %>%
    group_by(household_id) %>%
    mutate(oldest_member=max(AGE)) %>%
    ungroup()
  
  clean.choice.data <- clean.choice.data %>%
    filter(oldest_member<30 & !is.na(oldest_member))
  
    ## start back here...applying catastrophic coverage eligiblity
}


# Restrictions on who can pick catastrophic plans
# Household all under age 30
# No affordable offer

catastrophic_plans <- as.character(unique(plan_data[plan_data$metal_level == "Minimum Coverage","Plan_Name_Small"]))


# Affordable Offer


thresholds <- rep(affordability_threshold["2014"],nrow(households))
thresholds[which(households$year == 2015)] <- affordability_threshold["2015"]
thresholds[which(households$year == 2016)] <- affordability_threshold["2016"]
thresholds[which(households$year == 2017)] <- affordability_threshold["2017"]
thresholds[which(households$year == 2018)] <- affordability_threshold["2018"]
thresholds[which(households$year == 2019)] <- affordability_threshold["2019"]

unaffordable_exchange_offer_households_subsidized <- 
  which(((households$cheapest_premium - households$subsidy) * 12 > 
           thresholds * households$FPL * households$poverty_threshold) &
          households$subsidized_members > 0 & !households$flagged)
unaffordable_exchange_offer_households_unsubsidized <- 
  which((households$cheapest_premium * 12 > 
           thresholds * households$FPL * households$poverty_threshold )&
          households$subsidized_members == 0 & !households$flagged)


catastrophic_eligibles <- union(catastrophic_eligibles,
                                c(unaffordable_exchange_offer_households_subsidized,unaffordable_exchange_offer_households_unsubsidized))

choices[setdiff(1:nrow(choices),catastrophic_eligibles),catastrophic_plans] <- NA
new_choices[setdiff(1:nrow(new_choices),catastrophic_eligibles),catastrophic_plans] <- 0


# Input choice
# Complication with households choosing multiple plans
# Use choice of household head

plan_data <- rbind(plan_data,rep(NA,dim(plan_data)[2]))
plan_data$Plan_Name_Small <- as.character(plan_data$Plan_Name_Small)
plan_data[nrow(plan_data),"Plan_Name_Small"] <- "Uninsured"
#plan_data[nrow(plan_data),"plan_unique_id"] <- "Uninsured"
plan_data[nrow(plan_data),"plan_number_small"] <- ncol(choices) + 1
plan_data[nrow(plan_data),"plan_number"] <- max(plan_data$plan_number,na.rm=T) + 1
#rownames(plan_data)[nrow(plan_data)] <- "Uninsured"

assign_head_plan <- function(x) {
  return(as.integer(x[1]))
}

assign_oldest <- function(x) {
  return(as.character(x[1]))
}

households$plan_number <- as.numeric(by(data[,"plan_number"],data[,"household_year"],assign_head_plan)[rownames(households)])
households$plan_name <- by(data[,"plan_name"],data[,"household_year"],assign_oldest)[rownames(households)]
households$plan_id <- as.numeric(by(data[,"plan_id"],data[,"household_year"],assign_head_plan)[rownames(households)])
#households[households$plan_number == max(plan_data$plan_number),"plan_name"] <- "Uninsured"

households$plan_number_small <- reference_numbers[households$plan_number]
households[is.na(households$plan_id),"plan_number_small"] <- ncol(choices) + 1	

choices <- cbind(choices,rep(0,nrow(choices)))
colnames(choices) <- c(colnames(choices)[1:(ncol(choices)-1)],"Uninsured")
choices[cbind(seq(nrow(choices)),households$plan_number_small)] <- 1
gc()

new_choices <- cbind(new_choices,rep(0,nrow(new_choices)))
colnames(new_choices) <- c(colnames(new_choices)[1:(ncol(new_choices)-1)],"Uninsured")
gc()


  
  # These are the premiums before merging the small plans
  premium_plans <- plan_data[available_plans,"Premium"]/1.278
  names(premium_plans) <- available_plan_names
  
  
  # Here we need to do something about the small plans
  # Solution 1: take the minimum (what I do here)
  # Solution 2: average
  # NOTE: this ends up catching 2 Health Net plans (an HSP and PPO get merged) - no big deal
  
  small_plans <- unique(available_plan_names[duplicated(available_plan_names)])					
  for(s in small_plans) {
    plan_indices <- which(available_plan_names == s)
    plans_to_merge <- available_plans[plan_indices]
    premium_plans[plan_indices] <- min(plan_data[plans_to_merge,"Premium"]/1.278)
  }
  
  unique_available_plan_names <- which(!duplicated(available_plan_names))
  available_plan_names <- available_plan_names[unique_available_plan_names]
  premium_plans <- premium_plans[unique_available_plan_names]	
  
  households_to_update <- which(households$zip_region_year == i & !is.na(households$zip_region_year))
  
  premiums[households_to_update,available_plan_names] <- households[households_to_update,"rating_factor"] %*% t(premium_plans)
}

gc()

# Insert Mandate Penalty as the Price of Being Uninsured
# I am converting this penalty to a monthly basis 
premiums[,"Uninsured"] <- households$penalty/12
gc()

# Apply Subsidy to all plans except catastrophic
catastrophic_plans <- unique(plan_data[plan_data$metal_level == "Minimum Coverage","Plan_Name_Small"])
noncatastrophic_plans <- which(!colnames(premiums) %in% catastrophic_plans)
subsidies <- households$subsidy
subsidies[is.na(subsidies)] <- 0

unsubsidized_premiums <- premiums
gc()
premiums[,noncatastrophic_plans] <- pmax(premiums[,noncatastrophic_plans] - subsidies,0)
gc()

# Set premiums of plans not in choice set to NA
premiums <- premiums * pmax(pmin(choices,1),1)	
gc()
subsidies <- unsubsidized_premiums - premiums
gc()
rm(unsubsidized_premiums)
gc()
subsidies[,ncol(subsidies)] <- 0
gc()
