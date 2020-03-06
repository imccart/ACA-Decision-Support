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
  

data.clean <- data.clean %>%
  left_join(hh.clean %>% select(household_id, year, out_of_market), 
            by=c("household_id","year")) %>%
  filter(out_of_market == FALSE)


# Form choice sets --------------------------------------------------------




##### Choice Matrix (households by plans)
# 1 if household selected plan, 0 if household did not select plan, NA if plan not in choice set
# Complication: households that selected multiple plans

# NOTE: I am reordering plan data because I defined a plan_id number earlier based on a previous ordering
plan_data$Plan_ID_Order <- as.character(plan_data$Plan_ID_Order)
rownames(plan_data) <- paste(plan_data$Plan_Name,plan_data$ENROLLMENT_YEAR,
                             plan_data$region,sep="")
plan_data <- plan_data[plan_data$Plan_ID_Order,]

plan_names <- sort(unique(plan_data$Plan_Name2))
plan_numbers <- 1:length(plan_names)
names(plan_numbers) <- plan_names
#data$plan_number <- NA
#data[!is.na(data$plan_id),"plan_number"] <- plan_numbers[data[!is.na(data$plan_id),"plan_name"]]
data[is.na(data$plan_id),"plan_number"] <- max(data$plan_number,na.rm=TRUE) + 1
plan_data$plan_number <- plan_numbers[plan_data$Plan_Name2]

# Assign an integer to each plan (eliminating small plans
plan_names_small <- sort(unique(plan_data$Plan_Name_Small))
plan_numbers_small <- 1:length(plan_names_small)
names(plan_numbers_small) <- plan_names_small
#data$plan_number_small <- NA
#data[!is.na(data$plan_id),"plan_number_small"] <- plan_numbers_small[data[!is.na(data$plan_id),"plan_name"]]
#data[is.na(data$plan_id),"plan_number_small"] <- max(data$plan_number_small,na.rm=TRUE) + 1
plan_data$plan_number_small <- plan_numbers_small[plan_data$Plan_Name_Small]

# This will link the full plan number to the small plan number
reference_numbers <- plan_numbers
for(j in 1:length(reference_numbers)){
  reference_plan_name <- unique(plan_data[which(plan_data$Plan_Name == names(plan_numbers)[j]),"Plan_Name_Small"]) 
  reference_numbers[j] <- plan_numbers_small[reference_plan_name]
}

plan_data$Issuer_Name <- as.character(plan_data$Issuer_Name)
plan_data[plan_data$Issuer_Name == "Anthem Blue Cross","Issuer_Name"] <- "Anthem"
plan_data[plan_data$Issuer_Name == "Blue Shield","Issuer_Name"] <- "Blue_Shield"
plan_data[plan_data$Issuer_Name == "Chinese Community","Issuer_Name"] <- "Chinese_Community"
plan_data[plan_data$Issuer_Name == "Contra Costa Health Plan","Issuer_Name"] <- "Contra_Costa"
plan_data[plan_data$Issuer_Name == "Health Net","Issuer_Name"] <- "Health_Net"
plan_data[plan_data$Issuer_Name == "LA Care","Issuer_Name"] <- "LA_Care"
plan_data[plan_data$Issuer_Name == "UnitedHealthcare","Issuer_Name"] <- "United"
plan_data[plan_data$Issuer_Name == "Western Health","Issuer_Name"] <- "Western"
plan_data[plan_data$Issuer_Name == "Sharp","Issuer_Name"] <- "SHARP"

zipchoices <- as.matrix(zip3_choices[,4:ncol(zip3_choices)])
product_definitions$insurer <- as.character(product_definitions$insurer)
product_definitions$plan_network_type <- as.character(product_definitions$plan_network_type)
single_product_insurers <- c("Chinese_Community","Contra_Costa","Kaiser","LA_Care","Molina",
                             "Oscar","United","Valley","Western")
large_insurers <- c("Anthem","Blue_Shield","Kaiser","Health_Net","Molina")
#households$zip_region_year <- paste(households$zip_3,households$rating_area,households$year,sep="_")			

# Initialize choice and new plans matrix
choices <- matrix(NA,nrow=dim(households)[1],ncol=length(plan_names_small),dimnames=list(rownames(households),plan_names_small))	
new_choices <- matrix(0,nrow=dim(households)[1],ncol=length(plan_names_small),dimnames=list(rownames(households),plan_names_small))	

# Households
households$change_in_products <- 0
households$change_in_insurers <- 0
households$change_in_large_insurers <- 0
households$market_turnover <- 0

# Choices for the exchange population for which we have 3-digit zip (plan has to be offered in year and zip)
for(i in rownames(zip3_choices)) {
  
  t <- zip3_choices[i,"Year"]
  
  # Get the broad product categories available (insurer/network type/etc. - no metal)
  region_year_plans <- which(plan_data$ENROLLMENT_YEAR == t &
                               plan_data$region == zip3_choices[i,"Region"])
  available_products <- zipchoices[i,]
  available_products <- names(available_products)[!is.na(available_products)]
  available_insurers <- unique(product_definitions[available_products,"insurer"])
  large_available_insurers <- intersect(large_insurers,available_insurers)
  available_plans <- c()
  
  # Last year's plans
  if(t > 2014) {
    lastyr_zip <- paste(zip3_choices[i,"zip3"],zip3_choices[i,"Region"],t-1,sep="_")
    lastyr_region_year_plans <- which(plan_data$ENROLLMENT_YEAR == t-1 &
                                        plan_data$region == zip3_choices[lastyr_zip,"Region"])
    lastyr_available_products <- zipchoices[lastyr_zip,]
    lastyr_available_products <- names(lastyr_available_products)[!is.na(lastyr_available_products)]
    lastyr_available_insurers <- unique(product_definitions[lastyr_available_products,"insurer"])
    lastyr_large_available_insurers <- intersect(large_insurers,lastyr_available_insurers)
    lastyr_available_plans <- c()
  }
  
  # Now get the specific plans available
  
  if ("Anthem" %in% available_insurers) {
    if("Anthem_HMO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "HMO" &
                                             plan_data$MSP == 0)))
    }
    if("Anthem_EPO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "EPO" &
                                             plan_data$MSP == 0)))
    }
    if("Anthem_PPO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "PPO" &
                                             plan_data$MSP == 0)))
    }
    if("Anthem_EPO_MSP" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "EPO" &
                                             plan_data$MSP == 1)))
    }
    if("Anthem_PPO_MSP" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "PPO" &
                                             plan_data$MSP == 1)))
    }
  } 
  if ("Blue_Shield" %in% available_insurers) {
    # could be HMO, EPO, PPO
    if("Blue_Shield_HMO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Blue_Shield" & plan_data$PLAN_NETWORK_TYPE == "HMO" )))
    }
    if("Blue_Shield_EPO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Blue_Shield" & plan_data$PLAN_NETWORK_TYPE == "EPO" )))
    }
    if("Blue_Shield_PPO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Blue_Shield" & plan_data$PLAN_NETWORK_TYPE == "PPO" )))
    }
  } 
  if ("Health_Net" %in% available_insurers) {
    # could be HMO, HSP, EPO, PPO
    if("Health_Net_HMO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "HMO")))
    }
    if("Health_Net_HSP" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "HSP")))
    }
    if("Health_Net_EPO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "EPO")))
    }
    if("Health_Net_PPO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "PPO")))
    }
  }
  if ("SHARP" %in% available_insurers) {
    # could be network 1 or 2
    if("Sharp1" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "SHARP" & plan_data$Network_num == 1 & 
                                             !is.na(plan_data$Network_num))))
    }
    if("Sharp2" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "SHARP" & plan_data$Network_num == 2 & 
                                             !is.na(plan_data$Network_num))))
    }
  }
  if (any(single_product_insurers %in% available_insurers)) {
    available_plans <- c(available_plans,
                         intersect(region_year_plans,
                                   which(plan_data$Issuer_Name %in% intersect(available_insurers,single_product_insurers))))
  }
  
  households_to_update <- which(households$zip_region_year == i & !is.na(households$zip_region_year))
  available_plan_names <- unique(as.character(plan_data[available_plans,"Plan_Name_Small"]))
  choices[households_to_update,available_plan_names] <- 0
  
  if(t > 2014) {
    if(length(available_products) > length(lastyr_available_products)) {
      households[households_to_update,"change_in_products"] <- 1 # Net entry
    } else if (length(available_products) < length(lastyr_available_products)) {
      households[households_to_update,"change_in_products"] <- -1 # Net exit
    }
    
    if(length(available_insurers) > length(lastyr_available_insurers)) {
      households[households_to_update,"change_in_insurers"] <- 1 # Net entry
    } else if (length(available_insurers) < length(lastyr_available_insurers)) {
      households[households_to_update,"change_in_insurers"] <- -1 # Net exit
    }
    
    if(length(large_available_insurers) > length(lastyr_large_available_insurers)) {
      households[households_to_update,"change_in_large_insurers"] <- 1 # Net entry
    } else if (length(large_available_insurers) < length(lastyr_large_available_insurers)) {
      households[households_to_update,"change_in_large_insurers"] <- -1 # Net exit
    }
    
    lastyr_large_available_insurers
    
    number_in_market <- sum(households[!is.na(households$plan_number_nocsr) & households$zip_region_year == i &
                                         !is.na(households$zip_region_year),"weight"])
    
    if(number_in_market > 0) {
      households[households_to_update,"market_turnover"] <- 
        sum(households[is.na(households$previous_plan_number) & 
                         !is.na(households$plan_number_nocsr) & households$zip_region_year == i &
                         !is.na(households$zip_region_year),"weight"])/number_in_market
    }	
    
  }
  
  if(t > 2014) {
    if ("Anthem" %in% lastyr_available_insurers) {
      if("Anthem_HMO" %in% lastyr_available_products) {
        lastyr_available_plans <- c(lastyr_available_plans,
                                    intersect(lastyr_region_year_plans,
                                              which(plan_data$Issuer_Name == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "HMO" &
                                                      plan_data$MSP == 0)))
      }
      if("Anthem_EPO" %in% lastyr_available_products) {
        lastyr_available_plans <- c(lastyr_available_plans,
                                    intersect(lastyr_region_year_plans,
                                              which(plan_data$Issuer_Name == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "EPO" &
                                                      plan_data$MSP == 0)))
      }
      if("Anthem_PPO" %in% lastyr_available_products) {
        lastyr_available_plans <- c(lastyr_available_plans,
                                    intersect(lastyr_region_year_plans,
                                              which(plan_data$Issuer_Name == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "PPO" &
                                                      plan_data$MSP == 0)))
      }
      if("Anthem_EPO_MSP" %in% lastyr_available_products) {
        lastyr_available_plans <- c(lastyr_available_plans,
                                    intersect(lastyr_region_year_plans,
                                              which(plan_data$Issuer_Name == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "EPO" &
                                                      plan_data$MSP == 1)))
      }
      if("Anthem_PPO_MSP" %in% lastyr_available_products) {
        lastyr_available_plans <- c(lastyr_available_plans,
                                    intersect(lastyr_region_year_plans,
                                              which(plan_data$Issuer_Name == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "PPO" &
                                                      plan_data$MSP == 1)))
      }
    } 
    if ("Blue_Shield" %in% lastyr_available_insurers) {
      # could be HMO, EPO, PPO
      if("Blue_Shield_HMO" %in% lastyr_available_products) {
        lastyr_available_plans <- c(lastyr_available_plans,
                                    intersect(lastyr_region_year_plans,
                                              which(plan_data$Issuer_Name == "Blue_Shield" & plan_data$PLAN_NETWORK_TYPE == "HMO" )))
      }
      if("Blue_Shield_EPO" %in% lastyr_available_products) {
        lastyr_available_plans <- c(lastyr_available_plans,
                                    intersect(lastyr_region_year_plans,
                                              which(plan_data$Issuer_Name == "Blue_Shield" & plan_data$PLAN_NETWORK_TYPE == "EPO" )))
      }
      if("Blue_Shield_PPO" %in% lastyr_available_products) {
        lastyr_available_plans <- c(lastyr_available_plans,
                                    intersect(lastyr_region_year_plans,
                                              which(plan_data$Issuer_Name == "Blue_Shield" & plan_data$PLAN_NETWORK_TYPE == "PPO" )))
      }
    } 
    if ("Health_Net" %in% lastyr_available_insurers) {
      # could be HMO, HSP, EPO, PPO
      if("Health_Net_HMO" %in% lastyr_available_products) {
        lastyr_available_plans <- c(lastyr_available_plans,
                                    intersect(lastyr_region_year_plans,
                                              which(plan_data$Issuer_Name == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "HMO")))
      }
      if("Health_Net_HSP" %in% lastyr_available_products) {
        lastyr_available_plans <- c(lastyr_available_plans,
                                    intersect(lastyr_region_year_plans,
                                              which(plan_data$Issuer_Name == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "HSP")))
      }
      if("Health_Net_EPO" %in% lastyr_available_products) {
        lastyr_available_plans <- c(lastyr_available_plans,
                                    intersect(lastyr_region_year_plans,
                                              which(plan_data$Issuer_Name == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "EPO")))
      }
      if("Health_Net_PPO" %in% lastyr_available_products) {
        lastyr_available_plans <- c(lastyr_available_plans,
                                    intersect(lastyr_region_year_plans,
                                              which(plan_data$Issuer_Name == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "PPO")))
      }
    }
    if ("SHARP" %in% lastyr_available_insurers) {
      # could be network 1 or 2
      if("Sharp1" %in% lastyr_available_products) {
        lastyr_available_plans <- c(lastyr_available_plans,
                                    intersect(lastyr_region_year_plans,
                                              which(plan_data$Issuer_Name == "SHARP" & plan_data$Network_num == 1 & 
                                                      !is.na(plan_data$Network_num))))
      }
      if("Sharp2" %in% lastyr_available_products) {
        lastyr_available_plans <- c(lastyr_available_plans,
                                    intersect(lastyr_region_year_plans,
                                              which(plan_data$Issuer_Name == "SHARP" & plan_data$Network_num == 2 & 
                                                      !is.na(plan_data$Network_num))))
      }
    }
    if (any(single_product_insurers %in% lastyr_available_insurers)) {
      lastyr_available_plans <- c(lastyr_available_plans,
                                  intersect(lastyr_region_year_plans,
                                            which(plan_data$Issuer_Name %in% intersect(lastyr_available_insurers,single_product_insurers))))
    }
    
    lastyr_available_plan_names <- unique(as.character(plan_data[lastyr_available_plans,"Plan_Name_Small"]))
    new_plans <- setdiff(available_plan_names,lastyr_available_plan_names)
    new_choices[households_to_update,new_plans] <- 1 
    
  }
}

# Restrictions on who can pick ACS plans
# Must be subsidy eligible and have income below 250%
# "Unenhanced silver" is not in choice set of ACS-eligibles
# NOTE: this is not quite accurate if households have both subsidized and unsubsidized members

acs_eligibles73 <- which(households$FPL > 2 & households$FPL <= 2.5 & households$subsidized_members > 0)
acs_eligibles87 <- which(households$FPL > 1.5 & households$FPL <= 2 & households$subsidized_members > 0)
acs_eligibles94 <- which(households$FPL <= 1.5 & households$subsidized_members > 0)

acs_plans73 <- plan_data[plan_data$metal_level %in% c("Silver - Enhanced 73"),"Plan_Name_Small"]
acs_plans87 <- plan_data[plan_data$metal_level %in% c("Silver - Enhanced 87"),"Plan_Name_Small"]
acs_plans94 <- plan_data[plan_data$metal_level %in% c("Silver - Enhanced 94"),"Plan_Name_Small"]
silver_plans <- plan_data[plan_data$metal_level == "Silver","Plan_Name_Small"]

choices[setdiff(1:nrow(choices),acs_eligibles73),acs_plans73] <- NA 
choices[setdiff(1:nrow(choices),acs_eligibles87),acs_plans87] <- NA 
choices[setdiff(1:nrow(choices),acs_eligibles94),acs_plans94] <- NA 
choices[c(acs_eligibles73,acs_eligibles87,acs_eligibles94),silver_plans] <- NA

new_choices[setdiff(1:nrow(new_choices),acs_eligibles73),acs_plans73] <- 0 
new_choices[setdiff(1:nrow(new_choices),acs_eligibles87),acs_plans87] <- 0 
new_choices[setdiff(1:nrow(new_choices),acs_eligibles94),acs_plans94] <- 0 
new_choices[c(acs_eligibles73,acs_eligibles87,acs_eligibles94),silver_plans] <- 0


rm(acs_eligibles73,acs_eligibles87,acs_eligibles94,acs_plans73,acs_plans87,acs_plans94,silver_plans)
gc()

# Restrictions on who can pick catastrophic plans
# Household all under age 30
# No affordable offer

catastrophic_plans <- as.character(unique(plan_data[plan_data$metal_level == "Minimum Coverage","Plan_Name_Small"]))

# Age	
oldest_member <- by(data$AGE,data$household_year,max)
catastrophic_eligibles <- which(oldest_member < 30 & !is.na(oldest_member))

# Affordable Offer

affordability_threshold <- c(0.08,0.0805,0.0813,0.0816,0.0805,0.830)
names(affordability_threshold) <- c("2014","2015","2016","2017","2018","2019")

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


#write.csv(choices[,!colnames(choices) == "Uninsured"], file = "ca_choice_matrix_SEP012016.csv",row.names=FALSE)
#rm(choices_julia)
gc()

##### Previous plan choices

# Previous plan choice is already in household object
# You need to be careful with CSR plans (shift between CSR plans shouldn't count as change

households$previous_plan_number_small <- reference_numbers[households$previous_plan_number]


previous_choices <- matrix(0,nrow(choices),ncol(choices),dimnames=list(rownames(choices),colnames(choices)))

for(j in unique(households$previous_plan_number_small)) {
  
  # Get households that chose j in previous year
  households_on_j <- which(households$previous_plan_number_small == j)
  
  # Get all plans that would be considered equivalent to j
  j_plan_name <- colnames(choices)[j]
  equivalent_plans <- unique(plan_data[plan_data$Plan_Name_Small_NOCSR == j_plan_name,"plan_number_small"])
  
  # Input previous choice (all equivalent plans get a 1, will eliminate extras in make.julia.V2.r)
  previous_choices[households_on_j,equivalent_plans] <- 1
}

#previous_choices <- cbind(previous_choices,NA)
#colnames(previous_choices)[ncol(previous_choices)] <- "Uninsured"

##### Premium Object

#choices <- get(load("ca_choice_matrix_FEB082016"))

premiums <- choices

# Choices for the exchange population for which we have 3-digit zip (plan has to be offered in year and zip)
for(i in rownames(zip3_choices)) {
  
  region_year_plans <- which(plan_data$ENROLLMENT_YEAR == zip3_choices[i,"Year"] &
                               plan_data$region == zip3_choices[i,"Region"])
  
  # Get the broad product categories available (insurer/network type/etc. - no metal)
  available_products <- zipchoices[i,]
  available_products <- names(available_products)[!is.na(available_products)]
  available_insurers <- unique(product_definitions[available_products,"insurer"])
  
  # Now get the specific plans available
  
  available_plans <- c()
  if ("Anthem" %in% available_insurers) {
    if("Anthem_HMO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "HMO" &
                                             plan_data$MSP == 0)))
    }
    if("Anthem_EPO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "EPO" &
                                             plan_data$MSP == 0)))
    }
    if("Anthem_PPO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "PPO" &
                                             plan_data$MSP == 0)))
    }
    if("Anthem_EPO_MSP" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "EPO" &
                                             plan_data$MSP == 1)))
    }
    if("Anthem_PPO_MSP" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "PPO" &
                                             plan_data$MSP == 1)))
    }
  } 
  if ("Blue_Shield" %in% available_insurers) {
    # could be HMO, EPO, PPO
    if("Blue_Shield_HMO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Blue_Shield" & plan_data$PLAN_NETWORK_TYPE == "HMO" )))
    }
    if("Blue_Shield_EPO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Blue_Shield" & plan_data$PLAN_NETWORK_TYPE == "EPO" )))
    }
    if("Blue_Shield_PPO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Blue_Shield" & plan_data$PLAN_NETWORK_TYPE == "PPO" )))
    }
  } 
  if ("Health_Net" %in% available_insurers) {
    # could be HMO, HSP, EPO, PPO
    if("Health_Net_HMO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "HMO")))
    }
    if("Health_Net_HSP" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "HSP")))
    }
    if("Health_Net_EPO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "EPO")))
    }
    if("Health_Net_PPO" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "PPO")))
    }
  }
  if ("SHARP" %in% available_insurers) {
    # could be network 1 or 2
    if("Sharp1" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "SHARP" & plan_data$Network_num == 1 & 
                                             !is.na(plan_data$Network_num))))
    }
    if("Sharp2" %in% available_products) {
      available_plans <- c(available_plans,
                           intersect(region_year_plans,
                                     which(plan_data$Issuer_Name == "SHARP" & plan_data$Network_num == 2 & 
                                             !is.na(plan_data$Network_num))))
    }
  }
  if (any(single_product_insurers %in% available_insurers)) {
    available_plans <- c(available_plans,
                         intersect(region_year_plans,
                                   which(plan_data$Issuer_Name %in% intersect(available_insurers,single_product_insurers))))
  }
  
  available_plans <- unique(available_plans)
  available_plan_names <- as.character(plan_data[available_plans,"Plan_Name_Small"])
  
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

save(premiums,file="ca_premium_matrix_AUG032019_small")		
save(choices,file="ca_choice_matrix_AUG032019_small")	
save(new_choices,file="ca_new_choice_matrix_AUG032019_small")	
save(previous_choices,file="ca_previous_choice_matrix_AUG032019_small")	
save(subsidies,file="ca_subsidy_matrix_AUG032019_small")




rm(subsidies)
rm(premiums)
rm(previous_choices)
rm(new_choices)
gc()

##### Households Object

compute.number.unique <- function(x) {
  return(length(unique(x)))
}	

#households[households$plan_name == "Uninsured","plan_name"] <- NA	
households[is.na(households$previous_plan_offered),"previous_plan_offered"] <- 0	
households$enrolled_beg_year <- as.numeric(is.na(households$previous_plan_offered) & (households$SEP == 0 & !is.na(households$SEP)))

# Service Channel

data$channel <- as.character(data$service_channel)
data[data$channel %in% c("CIA","PBE"),"channel"] <- "Insurance Agent"
data[data$channel %in% c("SCR","CEW","CEC"),"channel"] <- "Other Assistance"
data[is.na(data$plan_id),"channel"] <- NA

assign_service_channel <- function(x) {
  unique_channel <- as.character(unique(x))
  if(length(unique_channel) == 1) {
    return(unique_channel)
  } else if ("Insurance Agent" %in%  x) {
    return("Insurance Agent")
  } else if ("Other Assistance" %in%  x) {
    return("Other Assistance")
  } else {
    return("Unassisted")
  }
}

service_channels <- by(data[!is.na(data$channel),"channel"],data[!is.na(data$channel),"household_year"],assign_service_channel)
households$channel <- NA
households[names(service_channels),"channel"] <- service_channels 

# Language spoken

data$language <- as.character(data$language_spoken)
data[data$language == "English","language"] <- "English"
data[data$language == "Spanish","language"] <- "Spanish"
data[!data$language %in% c("(nonres","English","Spanish"),"language"] <- "Other Language"
data[data$language == "(nonres","language"] <- NA

assign_language <- function(x) {
  unique_channel <- as.character(unique(x))
  if(length(unique_channel) == 1) {
    return(unique_channel)
  } else if ("English" %in%  x) {
    return("English")
  } else if ("Spanish" %in%  x) {
    return("Spanish")
  } else {
    return("Other Language")
  }
}

languages <- by(data[!is.na(data$language),"language"],data[!is.na(data$language),"household_year"],assign_language)
households$language <- NA
households[names(languages),"language"] <- languages 

# Mean age
mean_ages <- by(data$AGE,data$household_year,mean)
households[names(mean_ages),"mean_age"] <- mean_ages

# SEP - make sure no NA values (could be NA if uninsured)
households[is.na(households$plan_id),"SEP"] <- 0

# SLC_contribution - make sure no NA values
households[is.na(households$SLC_contribution),"SLC_contribution"] <- 
  households[is.na(households$SLC_contribution),"premiumSLC"] 

fields <- sort(c("plan_name","plan_id","plan_number","FPL","household_size","enrollees",
                 "perc_0to17","perc_18to25","perc_26to34","perc_35to44","perc_45to54","perc_55to64","perc_65plus","perc_asian","perc_black","perc_hispanic","perc_other","perc_white", 
                 "perc_male","zip3","zip_region_year","rating_area","year","rating_factor","subsidy","subsidized_members","premiumSLC","SLC_contribution",
                 "penalty","weight","exempt.belowfiling","exempt.unaffordable",
                 "dep_midyear","plan_number_nocsr","previous_plan_number","previous_plan_offered","SEP","enrolled_beg_year",
                 "channel","language","mean_age"))





# Save Data
households <- households[,fields]
gc()

save(households,file="ca_household_characteristics_AUG032019_small")	
write.csv(households,file="ca_household_characteristics_AUG032019_small.csv",row.names=FALSE)	


##### Instruments

plan_data$Insurer <- plan_data$Issuer_Name
plan_data$Issuer_Name <- NULL

plan_data$Year <- plan_data$ENROLLMENT_YEAR
plan_data$ENROLLMENT_YEAR <- NULL

plan_data$Metal_Level <- plan_data$metal_level
plan_data$metal_level <- NULL

# Hausman Instruments
# Use premiums charged by the firm in other markets for the same metal tier plan
# If firm only offers plans in 1 rating area, set instrument equal to 0
# First add instrument to the plan object
# Then create I * J matrix of instruments

plan_data$Plan_Name_Small <- as.character(plan_data$Plan_Name_Small)
plan_data$Insurer <- as.character(plan_data$Insurer)
plan_data$Metal_Level <- as.character(plan_data$Metal_Level)

compute_Hausman_instruments <- function(x,plan_data) {
  plan_to_check <- plan_data[x,]
  plans_other_markets <- rownames(plan_data[plan_data$Plan_Name_Small == plan_to_check$Plan_Name_Small & plan_data$Rating_Area != plan_to_check$Rating_Area &
                                              plan_data$Year == plan_to_check$Year & plan_data$Metal_Level == plan_to_check$Metal_Level,])
  
  if(length(plans_other_markets) == 0) {
    return(0)
  } else {
    return(mean(plan_data[plans_other_markets,"Premium"],na.rm=TRUE))
  }
}

plan_data$Hausman_instrument <- sapply(rownames(plan_data),FUN = compute_Hausman_instruments,plan_data)
plan_data["Uninsured","Hausman_instrument"] <- 0
plan_data$Hausman_missing <- as.numeric(plan_data$Hausman_instrument == 0)

# BLP instruments
# For a given plan j, use sum of non-premium plan characteristics for all other plans offered by the firm selling plan j
# For a given plan j, use sum of non-premium plan characteristics for plans offered by competitors

characteristics <- c("HMO","HSA","AV","MSP") 

# HMO
plan_data$HMO <- as.numeric(plan_data$PLAN_NETWORK_TYPE == "HMO")

# AV - minimum coverage is an approximation
AVs <- c(0.55,0.6,0.7,0.8,0.9,0.73,0.87,0.94)
names(AVs) <- c("Minimum Coverage","Bronze","Silver","Gold","Platinum","Silver - Enhanced 73","Silver - Enhanced 87","Silver - Enhanced 94")
plan_data$AV <- AVs[as.character(plan_data$Metal_Level)]

compute_BLP_instruments <- function(x,plan_data,characteristic,own=FALSE) {
  plan_to_check <- plan_data[x,]
  
  if(own) {
    comparison_plans <- setdiff(rownames(plan_data[plan_data$Insurer == plan_to_check$Insurer & 
                                                     plan_data$Rating_Area == plan_to_check$Rating_Area & plan_data$Year == plan_to_check$Year & 
                                                     !is.na(plan_data$Insurer),]),rownames(plan_to_check))
  } else {
    comparison_plans <- rownames(plan_data[plan_data$Insurer != plan_to_check$Insurer & 
                                             plan_data$Rating_Area == plan_to_check$Rating_Area & plan_data$Year == plan_to_check$Year & 
                                             !is.na(plan_data$Insurer),])	
  }
  
  return(mean(plan_data[comparison_plans,characteristic]))
}

plan_data$HSA_own <- sapply(rownames(plan_data),FUN = compute_BLP_instruments,plan_data,characteristic = "HSA",own=TRUE)
plan_data$HSA_other <- sapply(rownames(plan_data),FUN = compute_BLP_instruments,plan_data,characteristic = "HSA",own=FALSE)
plan_data$HMO_own <- sapply(rownames(plan_data),FUN = compute_BLP_instruments,plan_data,characteristic = "HMO",own=TRUE)
plan_data$HMO_other <- sapply(rownames(plan_data),FUN = compute_BLP_instruments,plan_data,characteristic = "HMO",own=FALSE)
plan_data$AV_own <- sapply(rownames(plan_data),FUN = compute_BLP_instruments,plan_data,characteristic = "AV",own=TRUE)
plan_data$AV_other <- sapply(rownames(plan_data),FUN = compute_BLP_instruments,plan_data,characteristic = "AV",own=FALSE)
plan_data$MSP_own <- sapply(rownames(plan_data),FUN = compute_BLP_instruments,plan_data,characteristic = "MSP",own=TRUE)
plan_data$MSP_other <- sapply(rownames(plan_data),FUN = compute_BLP_instruments,plan_data,characteristic = "MSP",own=FALSE)

plan_data$Small_Insurer <- 0
plan_data[!plan_data$Insurer %in% c("Anthem","Blue_Shield","Health_Net","Kaiser"),"Small_Insurer"] <- 1

plan_data$Anthem <- as.numeric(plan_data$Insurer == "Anthem")
plan_data$Blue_Shield <- as.numeric(plan_data$Insurer == "Blue_Shield")
#plan_data$Chinese_Community <- as.numeric(plan_data$Insurer == "Chinese_Community")
#plan_data$Contra_Costa <- as.numeric(plan_data$Insurer == "Contra_Costa")
plan_data$Health_Net <- as.numeric(plan_data$Insurer == "Health_Net")
plan_data$Kaiser <- as.numeric(plan_data$Insurer == "Kaiser")
#plan_data$LA_Care <- as.numeric(plan_data$Insurer == "LA_Care")
#plan_data$Molina <- as.numeric(plan_data$Insurer == "Molina")
#plan_data$Oscar <- as.numeric(plan_data$Insurer == "Oscar")
#plan_data$Sharp <- as.numeric(plan_data$Insurer == "SHARP")
#plan_data$United <- as.numeric(plan_data$Insurer == "United")
#plan_data$Valley <- as.numeric(plan_data$Insurer == "Valley")
#plan_data$Western_Health <- as.numeric(plan_data$Insurer == "Western")

plan_data$Catastrophic <- as.numeric(plan_data$Metal_Level == "Minimum Coverage")
plan_data$Bronze <- as.numeric(plan_data$Metal_Level == "Bronze")
plan_data$Gold <- as.numeric(plan_data$Metal_Level == "Gold")
plan_data$Platinum <- as.numeric(plan_data$Metal_Level == "Platinum")
plan_data$Silver <- as.numeric(plan_data$Metal_Level %in% c("Silver","Silver - Enhanced 73","Silver - Enhanced 87","Silver - Enhanced 94"))

# Change to 21-year old Premium
plan_data$Premium <- plan_data$Premium/1.278

##### Plan Object

julia_fields <- c("Plan_Name_Small","Insurer","Metal_Level","Premium","HSA",               
                  "MSP","HMO","AV",
                  "Anthem","Blue_Shield","Health_Net","Kaiser","Small_Insurer",
                  "Catastrophic","Bronze","Silver","Gold","Platinum")      
fields <- c(julia_fields,"HSA_own","HSA_other","HMO_own","HMO_other","AV_own","AV_other","Rating_Area","Year")

julia_plan_data <- plan_data[!duplicated(plan_data$Plan_Name_Small),julia_fields]
julia_plan_data <- julia_plan_data[1:(nrow(julia_plan_data)-1),]
julia_plan_data <- julia_plan_data[!julia_plan_data$Plan_Name_Small == "Uninsured",] 
rownames(julia_plan_data) <- julia_plan_data$Plan_Name_Small

julia_plan_data$Plan_Name <- julia_plan_data$Plan_Name_Small		
julia_plan_data$Plan_Name_Small <- NULL

save(plan_data,file="ca_plan_characteristics_AUG032019_small")	
write.csv(julia_plan_data,file="ca_plan_characteristics_AUG032019_small.csv",row.names=FALSE)
