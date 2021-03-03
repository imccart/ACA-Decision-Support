####################################################
### Make Data Objects for Exchange Data Analysis ###
####################################################

##### Objects we need to make

	# Choice object: households by plans, including uninsured 
		# 1 for selected plans
		# 0 for non-selected plans in choice set
		# NA for plans not in choice set
	# Previous choice object: households by plans
	# Premium object: households by plans
	# Households object: household characteristics
	# Plan object: plan characteristics


##### Load Data

data <- get(load("data/final/enrollment_data")) # "Cleaned" Covered California enrollment data
households <- get(load("data/final/household_data")) # "Cleaned" Covered California household data
plan_data <- get(load("data/final/plan_data")) # Covered California plan data
age_rating_factors <- read.csv("age_rating_factors.csv",row.names=1) # CCIIO default rating curve
poverty_guidelines <- read.csv("poverty_guidelines.csv",row.names=1) # Poverty guidelines
rating_areas <- read.csv("rating_areas.csv",row.names = 1) # California county-rating area mapping
zip3_choices <- read.csv("zip3_choices.csv",row.names = 1) # choice set by 3 digit zip and rating area
product_definitions <- read.csv("product_definitions.csv",row.names = 1) # definitions of column names in zip3_choices
contribution_percentages <- read.csv("contribution_percentages.csv",row.names = 1) # ACA contribution percentages
outside_logit <- get(load("sipp_logit"))
commissions_data <- read.csv("commission_input.csv")
counterfactual_sample <- FALSE
outside_sample <- TRUE


# Let's drop all flagged records
households <- households[!households$flagged,]
data <- data[!data$flagged,]

gc()

# Now we have to sample here - code blows up otherwise!

	set.seed(2)
	sample_size <- 25000
	if(outside_sample) sample_size <- 55000
	
	keep_records <- sort(sample(1:nrow(households),size=sample_size,replace=FALSE))
	households <- households[keep_records,]
	data <- data[data$household_year %in% rownames(households),]
	
	# Predict who left market
	if(outside_sample) {
		
		# Add variables
		
			# Income brackets
			households$FPL_bracket <- "138orless"
			households[households$FPL > 1.38 & households$FPL <= 2.50,"FPL_bracket"] <- "138to250"
			households[households$FPL > 2.50 & households$FPL <= 4,"FPL_bracket"] <- "250to400"
			households[households$FPL > 4,"FPL_bracket"] <- "400ormore"
			
			#households$FPL_bracket <- "138orless"
			#households[households$FPL > 1.38 & households$FPL <= 2.50 & !households$flagged,"FPL_bracket"] <- "138to250"
			#households[households$FPL > 2.50 & households$FPL <= 4 & !households$flagged,"FPL_bracket"] <- "250to400"
			#households[households$FPL > 4 & !households$flagged,"FPL_bracket"] <- "400ormore"
			
			# Age Brackets
			households$perc_18to34 <- households$perc_18to25 + households$perc_26to34
			households$perc_35to54 <- households$perc_35to44 + households$perc_45to54
			
			# Midyear transition
			#households$midyear_transition <- pmax(households$dep_midyear,households$SEP)
		
			# missing_ms <- sum(households[is.na(households$rating_area) | is.na(households$FPL),"enrollees"])
			#households <- households[!is.na(households$rating_area) & !is.na(households$FPL),]
		
		# Determine in/out of market
		households$out_of_market <- 0
		out_of_market_predictions <- as.numeric(predict(outside_logit,newdata=households[is.na(households$plan_number_nocsr),],type="response") >=
			runif(nrow(households[is.na(households$plan_number_nocsr),])))
		households[is.na(households$plan_number_nocsr),"out_of_market"] <- out_of_market_predictions
		
		# Remove records from market - About 26.8% of uninsured records are kept 
		households <- households[households$out_of_market == 0,]
		data <- data[data$household_year %in% rownames(households),]
	}
	
	gc()
	

##### Choice Matrix (households by plans)
	# 1 if household selected plan, 0 if household did not select plan, NA if plan not in choice set
	# Complication: households that selected multiple plans
	
	plan_names <- sort(unique(as.character(plan_data$Plan_Name2)))
	plan_numbers <- 1:length(plan_names)
	names(plan_numbers) <- plan_names
	
	plan_names_small <- sort(unique(plan_data$Plan_Name_Small))
	plan_numbers_small <- 1:length(plan_names_small)
	names(plan_numbers_small) <- plan_names_small
			
	
	# This will link the full plan number to the small plan number
	reference_numbers <- plan_numbers
	for(j in 1:length(reference_numbers)){
		reference_plan_name <- unique(plan_data[which(plan_data$Plan_Name == names(plan_numbers)[j]),"Plan_Name_Small"]) 
		reference_numbers[j] <- plan_numbers_small[reference_plan_name]
	}
	
	zipchoices <- as.matrix(zip3_choices[,4:ncol(zip3_choices)])
	product_definitions$insurer <- as.character(product_definitions$insurer)
	product_definitions$plan_network_type <- as.character(product_definitions$plan_network_type)
	single_product_insurers <- c("Chinese_Community","Contra_Costa","Kaiser","LA_Care","Molina",
				"Oscar","United","Valley","Western")
				
	# Initialize choice and new plans matrix
	choices <- matrix(NA,nrow=dim(households)[1],ncol=length(plan_names_small),dimnames=list(rownames(households),plan_names_small))	
	
	# Choices for the exchange population for which we have 3-digit zip (plan has to be offered in year and zip)
	for(i in rownames(zip3_choices)) {
		
			t <- zip3_choices[i,"Year"]
		
			# Get the broad product categories available (insurer/network type/etc. - no metal)
				
				# NOTE: I think you should have also included zip3 here, this should be fixed at some point
			
			region_year_plans <- which(plan_data$Year == t &
				plan_data$region == zip3_choices[i,"Region"])
			available_products <- zipchoices[i,]
			available_products <- names(available_products)[!is.na(available_products)]
			available_insurers <- unique(product_definitions[available_products,"insurer"])
			available_plans <- c()
			
			# Now get the specific plans available
						
			if ("Anthem" %in% available_insurers) {
				if("Anthem_HMO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "HMO" &
							plan_data$MSP == 0)))
				}
				if("Anthem_EPO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "EPO" &
							plan_data$MSP == 0)))
				}
				if("Anthem_PPO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "PPO" &
							plan_data$MSP == 0)))
				}
				if("Anthem_EPO_MSP" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "EPO" &
							plan_data$MSP == 1)))
				}
				if("Anthem_PPO_MSP" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "PPO" &
							plan_data$MSP == 1)))
				}
			} 
			if ("Blue_Shield" %in% available_insurers) {
				# could be HMO, EPO, PPO
				if("Blue_Shield_HMO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Blue_Shield" & plan_data$PLAN_NETWORK_TYPE == "HMO" )))
				}
				if("Blue_Shield_EPO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Blue_Shield" & plan_data$PLAN_NETWORK_TYPE == "EPO" )))
				}
				if("Blue_Shield_PPO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Blue_Shield" & plan_data$PLAN_NETWORK_TYPE == "PPO" )))
				}
			} 
			if ("Health_Net" %in% available_insurers) {
				# could be HMO, HSP, EPO, PPO
				if("Health_Net_HMO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "HMO")))
				}
				if("Health_Net_HSP" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "HSP")))
				}
				if("Health_Net_EPO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "EPO")))
				}
				if("Health_Net_PPO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "PPO")))
				}
			}
			if ("SHARP" %in% available_insurers) {
				# could be network 1 or 2
				if("Sharp1" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "SHARP" & plan_data$Network_num == 1 & 
							!is.na(plan_data$Network_num))))
				}
				if("Sharp2" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "SHARP" & plan_data$Network_num == 2 & 
							!is.na(plan_data$Network_num))))
				}
			}
			if (any(single_product_insurers %in% available_insurers)) {
				available_plans <- c(available_plans,
					intersect(region_year_plans,
						which(plan_data$Insurer %in% intersect(available_insurers,single_product_insurers))))
			}
			
			households_to_update <- which(households$zip_region_year == i & !is.na(households$zip_region_year))
			available_plan_names <- unique(as.character(plan_data[available_plans,"Plan_Name_Small"]))
			choices[households_to_update,available_plan_names] <- 0
	}
	
	# Restrictions on who can pick ACS plans
		# Must be subsidy eligible and have income below 250%
		# "Unenhanced silver" is not in choice set of ACS-eligibles
		# NOTE: this is not quite accurate if households have both subsidized and unsubsidized members
	
	acs_eligibles73 <- which(households$FPL > 2 & households$FPL <= 2.5 & households$subsidized_members > 0)
	acs_eligibles87 <- which(households$FPL > 1.5 & households$FPL <= 2 & households$subsidized_members > 0)
	acs_eligibles94 <- which(households$FPL <= 1.5 & households$subsidized_members > 0)
	
	acs_plans73 <- plan_data[plan_data$Metal_Level %in% c("Silver - Enhanced 73"),"Plan_Name_Small"]
	acs_plans87 <- plan_data[plan_data$Metal_Level %in% c("Silver - Enhanced 87"),"Plan_Name_Small"]
	acs_plans94 <- plan_data[plan_data$Metal_Level %in% c("Silver - Enhanced 94"),"Plan_Name_Small"]
	silver_plans <- plan_data[plan_data$Metal_Level == "Silver","Plan_Name_Small"]
	
	choices[setdiff(1:nrow(choices),acs_eligibles73),acs_plans73] <- NA 
	choices[setdiff(1:nrow(choices),acs_eligibles87),acs_plans87] <- NA 
	choices[setdiff(1:nrow(choices),acs_eligibles94),acs_plans94] <- NA 
	choices[c(acs_eligibles73,acs_eligibles87,acs_eligibles94),silver_plans] <- NA

	rm(acs_eligibles73,acs_eligibles87,acs_eligibles94,acs_plans73,acs_plans87,acs_plans94,silver_plans)
	gc()
	                
	# Restrictions on who can pick catastrophic plans
		# Household all under age 30
		# No affordable offer
	
		catastrophic_plans <- as.character(unique(plan_data[plan_data$Metal_Level == "Minimum Coverage","Plan_Name_Small"]))
	
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
		
	# Input choice
		# Complication with households choosing multiple plans
		# Use choice of household head
	
	plan_data <- rbind(plan_data,rep(NA,dim(plan_data)[2]))
	plan_data[nrow(plan_data),"Plan_Name_Small"] <- "Uninsured"
	plan_data[nrow(plan_data),"plan_number_small"] <- ncol(choices) + 1
	plan_data[nrow(plan_data),"plan_number"] <- max(plan_data$plan_number,na.rm=T) + 1
	
	assign_head_plan <- function(x) {
		return(as.integer(x[1]))
	}
	
	households$plan_number <- as.numeric(by(data[,"plan_number"],data[,"household_year"],assign_head_plan)[rownames(households)])	
	households$plan_number_small <- reference_numbers[households$plan_number]
	households[is.na(households$plan_id),"plan_number_small"] <- ncol(choices) + 1	
		
	choices <- cbind(choices,rep(0,nrow(choices)))
	colnames(choices) <- c(colnames(choices)[1:(ncol(choices)-1)],"Uninsured")
	choices[cbind(seq(nrow(choices)),households$plan_number_small)] <- 1
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
	
##### Premium Object

	premiums <- choices
	
	# Choices for the exchange population for which we have 3-digit zip (plan has to be offered in year and zip)
	for(i in rownames(zip3_choices)) {
		
			region_year_plans <- which(plan_data$Year == zip3_choices[i,"Year"] &
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
							which(plan_data$Insurer == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "HMO" &
							plan_data$MSP == 0)))
				}
				if("Anthem_EPO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "EPO" &
							plan_data$MSP == 0)))
				}
				if("Anthem_PPO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "PPO" &
							plan_data$MSP == 0)))
				}
				if("Anthem_EPO_MSP" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "EPO" &
							plan_data$MSP == 1)))
				}
				if("Anthem_PPO_MSP" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Anthem" & plan_data$PLAN_NETWORK_TYPE == "PPO" &
							plan_data$MSP == 1)))
				}
			} 
			if ("Blue_Shield" %in% available_insurers) {
				# could be HMO, EPO, PPO
				if("Blue_Shield_HMO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Blue_Shield" & plan_data$PLAN_NETWORK_TYPE == "HMO" )))
				}
				if("Blue_Shield_EPO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Blue_Shield" & plan_data$PLAN_NETWORK_TYPE == "EPO" )))
				}
				if("Blue_Shield_PPO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Blue_Shield" & plan_data$PLAN_NETWORK_TYPE == "PPO" )))
				}
			} 
			if ("Health_Net" %in% available_insurers) {
				# could be HMO, HSP, EPO, PPO
				if("Health_Net_HMO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "HMO")))
				}
				if("Health_Net_HSP" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "HSP")))
				}
				if("Health_Net_EPO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "EPO")))
				}
				if("Health_Net_PPO" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "Health_Net" & plan_data$PLAN_NETWORK_TYPE == "PPO")))
				}
			}
			if ("SHARP" %in% available_insurers) {
				# could be network 1 or 2
				if("Sharp1" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "SHARP" & plan_data$Network_num == 1 & 
							!is.na(plan_data$Network_num))))
				}
				if("Sharp2" %in% available_products) {
					available_plans <- c(available_plans,
						intersect(region_year_plans,
							which(plan_data$Insurer == "SHARP" & plan_data$Network_num == 2 & 
							!is.na(plan_data$Network_num))))
				}
			}
			if (any(single_product_insurers %in% available_insurers)) {
				available_plans <- c(available_plans,
					intersect(region_year_plans,
						which(plan_data$Insurer %in% intersect(available_insurers,single_product_insurers))))
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
	
##### Commissions Object

	# Issues: 1) renewal commissions only if you stay on same plan in next year, 2) should be 0 if unassisted, 3) baseline to Kaiser?

	commissions <- premiums * 0
	renewals <- premiums * 0
	
	for(year in c(2014:2019)) {
	
		households_to_update <- which(households$year == year)
		
		# Anthem
		anthem_plans <- unique(plan_data[plan_data$Insurer == "Anthem" & !is.na(plan_data$Insurer),"Plan_Name_Small"])
		commissions[households_to_update,anthem_plans] <- commissions_data[commissions_data$Insurer == "Anthem" & commissions_data$Type == "New",paste("Y",year,sep="")] 
		
		if(year > 2014) {
			previously_on_anthem <- rowSums(previous_choices[,anthem_plans])
			anthem_renewals <- which(households$year == year & previously_on_anthem >= 1)
			commissions[anthem_renewals,anthem_plans] <- commissions_data[commissions_data$Insurer == "Anthem" & commissions_data$Type == "Renewal",paste("Y",year,sep="")] 
			renewals[anthem_renewals,anthem_plans] <- 1
		}
		
		# Blue Shield
		blue_shield_PPO_plans <- unique(plan_data[plan_data$Insurer == "Blue_Shield" & 
			plan_data$PLAN_NETWORK_TYPE == "PPO" & !is.na(plan_data$Insurer),"Plan_Name_Small"])
		blue_shield_HMO_plans <- unique(plan_data[plan_data$Insurer == "Blue_Shield" & 
			plan_data$PLAN_NETWORK_TYPE == "HMO" & !is.na(plan_data$Insurer),"Plan_Name_Small"])
		commissions[households_to_update,blue_shield_PPO_plans] <- commissions_data[commissions_data$Insurer == "Blue Shield" & 
				commissions_data$Network == "PPO" & commissions_data$Type == "New",paste("Y",year,sep="")] *
				(premiums[households_to_update,blue_shield_PPO_plans] / households[households_to_update,"enrollees"])	
		if(year >= 2017) {
			commissions[households_to_update,blue_shield_HMO_plans] <- commissions_data[commissions_data$Insurer == "Blue Shield" & 
				commissions_data$Network == "HMO" & commissions_data$Type == "New",paste("Y",year,sep="")] *
				(premiums[households_to_update,blue_shield_HMO_plans] / households[households_to_update,"enrollees"])
		}
		
		if(year > 2014) {
			previously_on_BS_PPO <- rowSums(previous_choices[,blue_shield_PPO_plans])
			bs_PPO_renewals <- which(households$year == year & previously_on_BS_PPO >= 1)
			commissions[bs_PPO_renewals,blue_shield_PPO_plans] <- commissions_data[commissions_data$Insurer == "Blue Shield" & 
				commissions_data$Network == "PPO" & commissions_data$Type == "Renewal",paste("Y",year,sep="")] *
				(premiums[bs_PPO_renewals,blue_shield_PPO_plans] / households[bs_PPO_renewals,"enrollees"])
			renewals[bs_PPO_renewals,blue_shield_PPO_plans] <- 1
			
			if(year >= 2018) {
				previously_on_BS_HMO <- rowSums(previous_choices[,blue_shield_HMO_plans])
				bs_HMO_renewals <- which(households$year == year & previously_on_BS_HMO >= 1)
				commissions[bs_HMO_renewals,blue_shield_HMO_plans] <- commissions_data[commissions_data$Insurer == "Blue Shield" & 
					commissions_data$Network == "HMO" & commissions_data$Type == "Renewal",paste("Y",year,sep="")] *
					(premiums[bs_HMO_renewals,blue_shield_HMO_plans] / households[bs_HMO_renewals,"enrollees"])
				renewals[bs_HMO_renewals,blue_shield_HMO_plans] <- 1
			}
		}
		
		# Health Net
		health_net_PPO_plans <- unique(plan_data[plan_data$Insurer == "Health_Net" & 
			plan_data$PLAN_NETWORK_TYPE == "PPO" & !is.na(plan_data$Insurer),"Plan_Name_Small"])
		health_net_HMO_plans <- unique(plan_data[plan_data$Insurer == "Health_Net" & 
			plan_data$PLAN_NETWORK_TYPE == "HMO" & !is.na(plan_data$Insurer),"Plan_Name_Small"])
		if(year >= 2018) {
			commissions[households_to_update,health_net_PPO_plans] <- commissions_data[commissions_data$Insurer == "Health Net" & 
				commissions_data$Network == "PPO" & commissions_data$Type == "New",paste("Y",year,sep="")]
			commissions[households_to_update,health_net_HMO_plans] <- commissions_data[commissions_data$Insurer == "Health Net" & 
				commissions_data$Network == "HMO" & commissions_data$Type == "New",paste("Y",year,sep="")]
		} else {
			commissions[households_to_update,health_net_PPO_plans] <- commissions_data[commissions_data$Insurer == "Health Net" & 
				commissions_data$Network == "PPO" & commissions_data$Type == "New",paste("Y",year,sep="")] *
				(premiums[households_to_update,health_net_PPO_plans] / households[households_to_update,"enrollees"])
			commissions[households_to_update,health_net_HMO_plans] <- commissions_data[commissions_data$Insurer == "Health Net" & 
				commissions_data$Network == "HMO" & commissions_data$Type == "New",paste("Y",year,sep="")] *
				(premiums[households_to_update,health_net_HMO_plans] / households[households_to_update,"enrollees"])
		}
		
		if(year > 2014) {
			previously_on_hn_PPO <- rowSums(previous_choices[,health_net_PPO_plans])
			previously_on_hn_HMO <- rowSums(previous_choices[,health_net_HMO_plans])
			hn_PPO_renewals <- which(households$year == year & previously_on_hn_PPO >= 1)
			hn_HMO_renewals <- which(households$year == year & previously_on_hn_HMO >= 1)
			if(year >= 2018) {
				commissions[hn_PPO_renewals,health_net_PPO_plans] <- commissions_data[commissions_data$Insurer == "Health Net" & 
					commissions_data$Network == "PPO" & commissions_data$Type == "Renewal",paste("Y",year,sep="")]
				commissions[hn_HMO_renewals,health_net_HMO_plans] <- commissions_data[commissions_data$Insurer == "Health Net" & 
					commissions_data$Network == "HMO" & commissions_data$Type == "Renewal",paste("Y",year,sep="")]
				renewals[hn_PPO_renewals,health_net_PPO_plans] <- 1
				renewals[hn_HMO_renewals,health_net_HMO_plans] <- 1
			} else {
				commissions[hn_PPO_renewals,health_net_PPO_plans] <- commissions_data[commissions_data$Insurer == "Health Net" & 
					commissions_data$Network == "PPO" & commissions_data$Type == "Renewal",paste("Y",year,sep="")] *
					(premiums[hn_PPO_renewals,health_net_PPO_plans] / households[hn_PPO_renewals,"enrollees"])
				commissions[hn_HMO_renewals,health_net_HMO_plans] <- commissions_data[commissions_data$Insurer == "Health Net" & 
					commissions_data$Network == "HMO" & commissions_data$Type == "Renewal",paste("Y",year,sep="")] *
					(premiums[hn_HMO_renewals,health_net_HMO_plans] / households[hn_HMO_renewals,"enrollees"])
				renewals[hn_PPO_renewals,health_net_PPO_plans] <- 1
				renewals[hn_HMO_renewals,health_net_HMO_plans] <- 1
			}
		}	
		
		# Kaiser
		kaiser_plans <- unique(plan_data[plan_data$Insurer == "Kaiser" & !is.na(plan_data$Insurer),"Plan_Name_Small"])
		commissions[households_to_update,kaiser_plans] <- commissions_data[commissions_data$Insurer == "Kaiser" & commissions_data$Type == "New",paste("Y",year,sep="")] 
		
		if(year > 2014) {
			previously_on_kaiser <- rowSums(previous_choices[,kaiser_plans])
			kaiser_renewals <- which(households$year == year & previously_on_kaiser >= 1)
			commissions[kaiser_renewals,kaiser_plans] <- commissions_data[commissions_data$Insurer == "Kaiser" & commissions_data$Type == "Renewal",paste("Y",year,sep="")] 
			renewals[kaiser_renewals,kaiser_plans] <- 1
		}	
			
		# For the small insurers, we'll calculate a simple average of the available plans
		
		year_rows <- rownames(zip3_choices[zip3_choices$Year == year,])
		large_insurers <- c("Anthem","Blue_Shield","Kaiser","Health_Net")
		small_plans <- c("SMALL_BR","SMALL_BR_HSA","SMALL_CAT","SMALL_G","SMALL_P",
			"SMALL_SIL","SMALL_SIL73","SMALL_SIL87","SMALL_SIL94")
		
		for(i in year_rows) {
			
			available_products <- zipchoices[i,]
			available_products <- names(available_products)[!is.na(available_products)]
			available_insurers <- setdiff(unique(product_definitions[available_products,"insurer"]),large_insurers)
			
			prev_available_insurers <- c()
			if(year > 2014) {
				zip <- zip3_choices[i,"zip3"]
				region <- zip3_choices[i,"Region"]
				year <- zip3_choices[i,"Year"]
				prev_available_products <- zipchoices[paste(zip,region,year-1,sep="_"),]
				prev_available_products <- names(prev_available_products)[!is.na(prev_available_products)]
				prev_available_insurers <- setdiff(unique(product_definitions[prev_available_products,"insurer"]),large_insurers)
			}	
			
			households_to_update <- which(households$zip_region_year == i & !is.na(households$zip_region_year))
			previously_on_small <- rowSums(previous_choices[,small_plans])
			small_renewals <- intersect(households_to_update,which(households$year == year & previously_on_small >= 1))	
			households_to_update <- setdiff(households_to_update,small_renewals)
			renewals[small_renewals,small_plans] <- 1
			
			if(length(available_insurers) > 0) {
				
				# Chinese Community
				if("Chinese_Community" %in% available_insurers) {
					commissions[households_to_update,small_plans] <- commissions[households_to_update,small_plans] + 
						commissions_data[commissions_data$Insurer == "Chinese Community" & commissions_data$Type == "New",paste("Y",year,sep="")]/length(available_insurers)
					if("Chinese_Community" %in% prev_available_insurers) {
						commissions[small_renewals,small_plans] <- commissions[small_renewals,small_plans] + 
							commissions_data[commissions_data$Insurer == "Chinese Community" & commissions_data$Type == "Renewal",paste("Y",year,sep="")] /length(available_insurers)
					}
				}
				if("Contra_Costa" %in% available_insurers) { # don't have contra costa commissions, just assume same as Kaiser
					commissions[households_to_update,small_plans] <- commissions[households_to_update,small_plans] + 
						commissions_data[commissions_data$Insurer == "Kaiser" & commissions_data$Type == "New",paste("Y",year,sep="")]/length(available_insurers)
				}
				if("Oscar" %in% available_insurers) {
					commissions[households_to_update,small_plans] <- commissions[households_to_update,small_plans] + 
						commissions_data[commissions_data$Insurer == "Oscar" & commissions_data$Type == "New",paste("Y",year,sep="")]/length(available_insurers)
					if("Oscar" %in% prev_available_insurers) {
						commissions[small_renewals,small_plans] <- commissions[small_renewals,small_plans] + 
							commissions_data[commissions_data$Insurer == "Oscar" & commissions_data$Type == "Renewal",paste("Y",year,sep="")]/length(available_insurers)
					}
				}	
				if("United" %in% available_insurers) {
					commissions[households_to_update,small_plans] <- commissions[households_to_update,small_plans] + 
						commissions_data[commissions_data$Insurer == "United" & commissions_data$Type == "New",paste("Y",year,sep="")]/length(available_insurers)
				}
				if("LA_Care" %in% available_insurers) {
					commissions[households_to_update,small_plans] <- commissions[households_to_update,small_plans] + 
						commissions_data[commissions_data$Insurer == "LA Care" & commissions_data$Type == "New",paste("Y",year,sep="")]/length(available_insurers)
					if("LA_Care" %in% prev_available_insurers) {
						commissions[small_renewals,small_plans] <- commissions[small_renewals,small_plans] + 
							commissions_data[commissions_data$Insurer == "LA Care" & commissions_data$Type == "Renewal",paste("Y",year,sep="")]/length(available_insurers)
					}
				}
				if("Molina" %in% available_insurers) {
					commissions[households_to_update,small_plans] <- commissions[households_to_update,small_plans] + 
						commissions_data[commissions_data$Insurer == "Molina" & commissions_data$Type == "New",paste("Y",year,sep="")]/length(available_insurers)
					if("Molina" %in% prev_available_insurers) {
						commissions[small_renewals,small_plans] <- commissions[small_renewals,small_plans] + 
							commissions_data[commissions_data$Insurer == "Molina" & commissions_data$Type == "Renewal",paste("Y",year,sep="")]/length(available_insurers)
					}
				}
				if("Valley" %in% available_insurers) {
					commissions[households_to_update,small_plans] <- commissions[households_to_update,small_plans] + 
						commissions_data[commissions_data$Insurer == "Valley" & commissions_data$Type == "New",paste("Y",year,sep="")]/length(available_insurers)
					if("Valley" %in% prev_available_insurers) {
					commissions[small_renewals,small_plans] <- commissions[small_renewals,small_plans] + 
						commissions_data[commissions_data$Insurer == "Valley" & commissions_data$Type == "Renewal",paste("Y",year,sep="")]/length(available_insurers)
					}
				}
				if("Western" %in% available_insurers) {
					commissions[households_to_update,small_plans] <- commissions[households_to_update,small_plans] + 
						commissions_data[commissions_data$Insurer == "Western" & commissions_data$Type == "New",paste("Y",year,sep="")]/length(available_insurers)
					if("Western" %in% prev_available_insurers) {
						commissions[small_renewals,small_plans] <- commissions[small_renewals,small_plans] + 
							commissions_data[commissions_data$Insurer == "Western" & commissions_data$Type == "Renewal",paste("Y",year,sep="")]/length(available_insurers)
					}
				}
				
				
				if("SHARP" %in% available_insurers) {
					commissions[households_to_update,small_plans] <- commissions[households_to_update,small_plans] + 
						commissions_data[commissions_data$Insurer == "Sharp" & commissions_data$Type == "New",paste("Y",year,sep="")] *
						(premiums[households_to_update,small_plans] / households[households_to_update,"enrollees"])/length(available_insurers)
					if("SHARP" %in% prev_available_insurers) {
						commissions[small_renewals,small_plans] <- commissions[small_renewals,small_plans] + 
							commissions_data[commissions_data$Insurer == "Sharp" & commissions_data$Type == "Renewal",paste("Y",year,sep="")] *
							(premiums[small_renewals,small_plans] / households[small_renewals,"enrollees"])/length(available_insurers)
					}
				}
			}
		}
	}
	
	# Set renewal variable to -1 if no broker used
	#renewals[households$broker == 0,] <- -1
	
	# Zero out commission if unassisted
	
		#commissions <- commissions * households$broker
	
	# Insert Mandate Penalty as the Price of Being Uninsured
	# I am converting this penalty to a monthly basis 
	premiums[,"Uninsured"] <- households$penalty/12
	gc()
	
	# Apply Subsidy to all plans except catastrophic
	catastrophic_plans <- unique(plan_data[plan_data$Metal_Level == "Minimum Coverage","Plan_Name_Small"])
	noncatastrophic_plans <- which(!colnames(premiums) %in% catastrophic_plans)
	subsidies <- households$subsidy
	subsidies[is.na(subsidies)] <- 0
	
	unsubsidized_premiums <- premiums
	gc()
	premiums[,noncatastrophic_plans] <- pmax(premiums[,noncatastrophic_plans] - subsidies,0)
	gc()
	
	# Set premiums and commissions of plans not in choice set to NA
	premiums <- premiums * pmax(pmin(choices,1),1)	
	commissions <- commissions * pmax(pmin(choices,1),1)	
	renewals <- renewals * pmax(pmin(choices,1),1)	
	gc()
	subsidies <- unsubsidized_premiums - premiums
	gc()
	rm(unsubsidized_premiums)
	gc()
	subsidies[,ncol(subsidies)] <- 0
	gc()

	save(premiums,file="ca_premium_matrix_FEB052021_small")		
	save(commissions,file="ca_commissions_matrix_FEB052021_small")	
	save(renewals,file="ca_renewals_matrix_FEB052021_small")			
	save(choices,file="ca_choice_matrix_FEB052021_small")	
	save(previous_choices,file="ca_previous_choice_matrix_FEB052021_small")	
	save(subsidies,file="ca_subsidy_matrix_FEB052021_small")
	
rm(subsidies)
rm(commissions)
rm(premiums)
rm(previous_choices)
gc()
	
##### Households Object

	compute.number.unique <- function(x) {
		return(length(unique(x)))
	}	

	#households[households$plan_name == "Uninsured","plan_name"] <- NA	
	households[is.na(households$previous_plan_offered),"previous_plan_offered"] <- 0	
	households$enrolled_beg_year <- as.numeric(is.na(households$previous_plan_offered) & (households$SEP == 0 & !is.na(households$SEP)))
	
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
		"agent","broker","navigator","language","mean_age"))
		
	# Save Data
	save(households,file="ca_household_characteristics_FEB052021_small")	
	write.csv(households,file="ca_household_characteristics_FEB052021_small.csv",row.names=FALSE)	
	
	
##### Instruments

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
		
save(plan_data,file="ca_plan_characteristics_FEB052021_small")	
write.csv(julia_plan_data,file="ca_plan_characteristics_FEB052021_small.csv",row.names=FALSE)
	