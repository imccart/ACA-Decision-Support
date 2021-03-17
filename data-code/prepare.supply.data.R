# Prepare Data for Supply Model

	# Data needed:
		# Pricing factors (IJ * 1)
		# Cost factors (IJ * 1)
		# Risk adjustment factors (J * 1, but probably just make IJ * 1)
		# Base Premiums (F * 1)
		# Base Costs (F * 1)

##### Load Data
	
	mlr_data <- read.csv("data/final/mlr_data.csv",header=TRUE,row.names=1)
	#rate_data <- read.csv("ca_risk_adj_results.csv")
	rate_data <- read.csv("convergence_analysis_updated.csv",row.names=1)
	
	julia_data <- read.csv(file="ca_julia_data_AUG032019_small.csv",header=TRUE)	
	households <- get(load("ca_household_characteristics_AUG032019_small"))
		
	data <- get(load("ca_enrollment_data_AUG012019")) # "Cleaned" Covered California enrollment data
	data <- data[data$household_year %in% rownames(households),]
	gc()
		
	plans = read.csv("ca_plan_characteristics_AUG032019_small.csv", header = TRUE) # high-level plans
	rownames(plans) <- plans$Plan_Name
		
	plan_data <- read.csv("ca_plan_data2.csv") # Covered California plan data by year and rating area
	plan_data$Plan_ID_Order <- as.character(plan_data$Plan_ID_Order)
	rownames(plan_data) <- paste(plan_data$Plan_Name,plan_data$ENROLLMENT_YEAR,
		plan_data$region,sep="")
	plan_data <- plan_data[plan_data$Plan_ID_Order,]
			
	zip3_choices <- read.csv("zip3_choices2.csv",row.names = 1) # choice set by 3 digit zip and rating area
	product_definitions <- read.csv("product_definitions.csv",row.names = 1) # definitions of column names in zip3_choices
	age_rating_factors <- read.csv("age_rating_factors.csv",row.names=1) # CCIIO default rating curve
	commissions_data <- read.csv("commission_input.csv")	
		
##### Julia demand data

	# want to eliminate CSR plans
	# need to add individual-specific AV
	
	julia_data$plan_name <- as.character(julia_data$plan_name)
	julia_data$AV <- plans[julia_data$plan_name,"AV"]
	julia_data[is.na(julia_data$AV),"AV"] <- 0
	julia_data[julia_data$plan_name == "Uninsured","AV"] <- 0	
	
	plans$Insurer <- as.character(plans$Insurer)
	plans[plans$Insurer == "Western","Insurer"] <- "Small_Insurer"
	
	# Insurer, HMO
	
	insurers <- c("Anthem","Blue_Shield","Kaiser","Health_Net","Small_Insurer")
	network_types <- c("HMO","PPO")
	
	plans$Plan_Name_NOCSR <- plans$Plan_Name
	julia_data$plan_name_nocsr <- julia_data$plan_name
	
	# Eliminate CSR plans
	
	for(f in insurers) {
	
		consolidate_plans <- which(plans$Insurer == f & plans$HMO == 1 & plans$Silver == 1)
		new_plan_name <- rownames(plans)[consolidate_plans[1]]
		plans[consolidate_plans,"Plan_Name_NOCSR"] <- new_plan_name
		julia_data[julia_data$plan_name %in% rownames(plans)[consolidate_plans],
			"plan_name_nocsr"] <- new_plan_name
		
		if(!f %in% c("Kaiser","Small_Insurer")) {
			consolidate_plans <- which(plans$Insurer == f & plans$HMO == 0 & plans$Silver == 1)
			new_plan_name <- rownames(plans)[consolidate_plans[1]]
			plans[consolidate_plans,"Plan_Name_NOCSR"] <- new_plan_name
			julia_data[julia_data$plan_name %in% rownames(plans)[consolidate_plans],
				"plan_name_nocsr"] <- new_plan_name
		}
	}
	
	# Make sure there are no duplicates (I found 4?)
	household_plan_ids <- paste(julia_data$plan_name_nocsr,julia_data$household_number,sep="_")
	dups <- household_plan_ids[which(duplicated(household_plan_ids))]
	delete_ids <- c()
	for(h in dups) {
		delete_ids <- c(delete_ids,which(household_plan_ids == h & julia_data$AV == 0.7))
	}
	julia_data <- julia_data[setdiff(1:nrow(julia_data),delete_ids),]
	
	plans <- plans[!duplicated(plans$Plan_Name_NOCSR),]
	plans <- plans[sort(rownames(plans)),]
	
	# Eliminate Catastrophic Plans and HSA plans and Kaiser Gold Coins
	
		# Reassign ANT_BR_HSA and ANT_CAT to ANT_BR 
		reassign_indices <- which(julia_data$plan_name %in% c("ANT_BR_HSA","ANT_CAT") & julia_data$choice == 1)
		reassign_households <- julia_data[reassign_indices,"household_number"]
		for(h in reassign_households) {
			julia_data[julia_data$household_number == h & julia_data$plan_name == "ANT_BR","choice"] <- 1
		}
		reassign_indices_prev <- which(julia_data$plan_name %in% c("ANT_BR_HSA","ANT_CAT") & julia_data$previous_choice == 1)
		reassign_households_prev <- julia_data[reassign_indices_prev,"household_number"]
		for(h in reassign_households_prev) {
			julia_data[julia_data$household_number == h & julia_data$plan_name == "ANT_BR","previous_choice"] <- 1
		}
		julia_data <- julia_data[!julia_data$plan_name %in% c("ANT_BR_HSA","ANT_CAT"),] 
	
		# Reassign BS_BR_HSA and BS_CAT to BS_BR 
		reassign_indices <- which(julia_data$plan_name %in% c("BS_BR_HSA","BS_CAT") & julia_data$choice == 1)
		reassign_households <- julia_data[reassign_indices,"household_number"]
		for(h in reassign_households) {
			julia_data[julia_data$household_number == h & julia_data$plan_name == "BS_BR","choice"] <- 1
		}
		reassign_indices_prev <- which(julia_data$plan_name %in% c("BS_BR_HSA","BS_CAT") & julia_data$previous_choice == 1)
		reassign_households_prev <- julia_data[reassign_indices_prev,"household_number"]
		for(h in reassign_households_prev) {
			julia_data[julia_data$household_number == h & julia_data$plan_name == "BS_BR","previous_choice"] <- 1
		}
		julia_data <- julia_data[!julia_data$plan_name %in% c("BS_BR_HSA","BS_CAT"),] 
	
		# Reassign KA_BR_HSA and KA_CAT to KA_BR 
		reassign_indices <- which(julia_data$plan_name %in% c("KA_BR_HSA","KA_CAT") & julia_data$choice == 1)
		reassign_households <- julia_data[reassign_indices,"household_number"]
		for(h in reassign_households) {
			julia_data[julia_data$household_number == h & julia_data$plan_name == "KA_BR","choice"] <- 1
		}
		reassign_indices_prev <- which(julia_data$plan_name %in% c("KA_BR_HSA","KA_CAT") & julia_data$previous_choice == 1)
		reassign_households_prev <- julia_data[reassign_indices_prev,"household_number"]
		for(h in reassign_households_prev) {
			julia_data[julia_data$household_number == h & julia_data$plan_name == "KA_BR","previous_choice"] <- 1
		}
		julia_data <- julia_data[!julia_data$plan_name %in% c("KA_BR_HSA","KA_CAT"),] 
	
		# Reassign HN_BR_HSA and HN_CAT to HN_BR 
		reassign_indices <- which(julia_data$plan_name %in% c("HN_BR_HSA","HN_CAT") & julia_data$choice == 1)
		reassign_households <- julia_data[reassign_indices,"household_number"]
		for(h in reassign_households) {
			julia_data[julia_data$household_number == h & julia_data$plan_name == "HN_BR","choice"] <- 1
		}
		reassign_indices_prev <- which(julia_data$plan_name %in% c("HN_BR_HSA","HN_CAT") & julia_data$previous_choice == 1)
		reassign_households_prev <- julia_data[reassign_indices_prev,"household_number"]
		for(h in reassign_households_prev) {
			julia_data[julia_data$household_number == h & julia_data$plan_name == "HN_BR","previous_choice"] <- 1
		}
		julia_data <- julia_data[!julia_data$plan_name %in% c("HN_BR_HSA","HN_CAT"),] 
	
		# Reassign SMALL_BR_HSA and SMALL_CAT to SMALL_BR 
		reassign_indices <- which(julia_data$plan_name %in% c("SMALL_BR_HSA","SMALL_CAT") & julia_data$choice == 1)
		reassign_households <- julia_data[reassign_indices,"household_number"]
		for(h in reassign_households) {
			julia_data[julia_data$household_number == h & julia_data$plan_name == "SMALL_BR","choice"] <- 1
		}
		reassign_indices_prev <- which(julia_data$plan_name %in% c("SMALL_BR_HSA","SMALL_CAT") & julia_data$previous_choice == 1)
		reassign_households_prev <- julia_data[reassign_indices_prev,"household_number"]
		for(h in reassign_households_prev) {
			julia_data[julia_data$household_number == h & julia_data$plan_name == "SMALL_BR","previous_choice"] <- 1
		}
		julia_data <- julia_data[!julia_data$plan_name %in% c("SMALL_BR_HSA","SMALL_CAT"),] 
	
	
		# Reassign HN_CAT3 to HN_BR3 
		reassign_indices <- which(julia_data$plan_name %in% c("HN_CAT3") & julia_data$choice == 1)
		reassign_households <- julia_data[reassign_indices,"household_number"]
		for(h in reassign_households) {
			julia_data[julia_data$household_number == h & julia_data$plan_name == "HN_BR3","choice"] <- 1
		}
		reassign_indices_prev <- which(julia_data$plan_name %in% c("HN_CAT3") & julia_data$previous_choice == 1)
		reassign_households_prev <- julia_data[reassign_indices_prev,"household_number"]
		for(h in reassign_households_prev) {
			julia_data[julia_data$household_number == h & julia_data$plan_name == "HN_BR3","previous_choice"] <- 1
		}
		julia_data <- julia_data[!julia_data$plan_name %in% c("HN_CAT3"),] 
	
		# Reassign KA_G_COIN to KA_G
			# Coinsurance plan started in 2017
			# Coinsurance plan is cheaper and has higher enrollment (use coinsurance premium)
		reassign_indices <- which(julia_data$plan_name %in% c("KA_G_COIN") & julia_data$choice == 1)
		reassign_households <- julia_data[reassign_indices,"household_number"]
		for(h in reassign_households) {
			julia_data[julia_data$household_number == h & julia_data$plan_name == "KA_G","choice"] <- 1
			julia_data[julia_data$household_number == h & julia_data$plan_name == "KA_G","premium"] <- 
				julia_data[julia_data$household_number == h & julia_data$plan_name == "KA_G_COIN","premium"]
		}
		reassign_indices_prev <- which(julia_data$plan_name %in% c("KA_G_COIN") & julia_data$previous_choice == 1)
		reassign_households_prev <- julia_data[reassign_indices_prev,"household_number"]
		for(h in reassign_households_prev) {
			julia_data[julia_data$household_number == h & julia_data$plan_name == "KA_G","previous_choice"] <- 1
		}
		julia_data <- julia_data[!julia_data$plan_name %in% c("KA_G_COIN"),] 
	
	plans <- plans[!plans$AV == 0.55,]
	plans <- plans[plans$HSA == 0,]
	plans <- plans[!plans$Plan_Name == "KA_G_COIN",]
	plans$HSA <- NULL
	plans$MSP <- NULL
	
##### Clean up plan data
	
		plan_data$Insurer <- plan_data$Issuer_Name
		plan_data$Insurer <- as.character(plan_data$Insurer)
		plan_data[plan_data$Insurer == "Anthem Blue Cross","Insurer"] <- "Anthem"
		plan_data[plan_data$Insurer == "Blue Shield","Insurer"] <- "Blue_Shield"
		plan_data[plan_data$Insurer == "Chinese Community","Insurer"] <- "Chinese_Community"
		plan_data[plan_data$Insurer == "Contra Costa Health Plan","Insurer"] <- "Contra_Costa"
		plan_data[plan_data$Insurer == "Health Net","Insurer"] <- "Health_Net"
		plan_data[plan_data$Insurer == "LA Care","Insurer"] <- "LA_Care"
		plan_data[plan_data$Insurer == "Western Health","Insurer"] <- "Western_Health"
	
		plan_data$Year <- plan_data$ENROLLMENT_YEAR
		plan_data$Metal_Level <- plan_data$metal_level
		plan_data$Type <- plan_data$PLAN_NETWORK_TYPE
	
		# Assign an integer to each plan
		
		data$plan_unique_id <- paste(data$plan_name,data$year,data$rating_area,sep="")
		
		plan_names <- sort(unique(plan_data$Plan_Name_Small_NOHSACAT))
		plan_numbers <- 1:length(plan_names)
		names(plan_numbers) <- plan_names
		
		plan_data$Plan_Name_Small_NOHSACAT <- as.character(plan_data$Plan_Name_Small_NOHSACAT)
		data$Plan_Name_Small_NOHSACAT <- plan_data[data$plan_id,"Plan_Name_Small_NOHSACAT"]
			
		data$plan_number <- NA
		data[!is.na(data$plan_id),"plan_number"] <- plan_numbers[data[!is.na(data$Plan_Name_Small_NOHSACAT),"Plan_Name_Small_NOHSACAT"]]
		data[is.na(data$plan_id),"plan_number"] <- max(data$plan_number,na.rm=TRUE) + 1
		plan_data$plan_number <- plan_numbers[plan_data$Plan_Name_Small_NOHSACAT]
	
		rate_data$insurer_year <- rownames(rate_data)
		data$insurer_small <- data$insurer
		data[!data$insurer_small %in% insurers & !is.na(data$insurer),"insurer_small"] <- "Small_Insurer"
		
		# Convert 40-year old premium to 21 year-old premium
		plan_data$Premium <- plan_data$Premium/1.278		
	
	
##### Clean up MLR Data		
		
		mlr_data[is.na(mlr_data$HIOS_ISSUER_ID),"HIOS_ISSUER_ID"] <- 99999
		mlr_data$GROUP_AFFILIATION <- as.character(mlr_data$GROUP_AFFILIATION)
		mlr_data[mlr_data$HIOS_ISSUER_ID == 27603,"GROUP_AFFILIATION"] <- "Anthem"
		mlr_data[mlr_data$HIOS_ISSUER_ID == 70285,"GROUP_AFFILIATION"] <- "Blue_Shield"
		mlr_data[mlr_data$HIOS_ISSUER_ID == 47579,"GROUP_AFFILIATION"] <- "Chinese_Community"
		mlr_data[mlr_data$HIOS_ISSUER_ID == 99483,"GROUP_AFFILIATION"] <- "Contra_Costa"
		mlr_data[mlr_data$HIOS_ISSUER_ID %in% c(67138,99110),"GROUP_AFFILIATION"] <- "Health_Net"
		mlr_data[mlr_data$HIOS_ISSUER_ID == 40513,"GROUP_AFFILIATION"] <- "Kaiser"
		mlr_data[mlr_data$HIOS_ISSUER_ID == 92815,"GROUP_AFFILIATION"] <- "LA_Care"
		mlr_data[mlr_data$HIOS_ISSUER_ID == 18126,"GROUP_AFFILIATION"] <- "Molina"
		mlr_data[mlr_data$HIOS_ISSUER_ID == 92499,"GROUP_AFFILIATION"] <- "Sharp"
		mlr_data[mlr_data$HIOS_ISSUER_ID == 84014,"GROUP_AFFILIATION"] <- "Valley"
		mlr_data[mlr_data$HIOS_ISSUER_ID == 93689,"GROUP_AFFILIATION"] <- "Western_Health"
		mlr_data[is.na(mlr_data$HIOS_ISSUER_ID),"GROUP_AFFILIATION"] <- "Small_Insurer"
			
		# Remove small insurers
		mlr_data <- mlr_data[mlr_data$GROUP_AFFILIATION %in% insurers,]
			
		# Combine the two Health Nets		
		for (t in min(mlr_data$year):max(mlr_data$year)) {		
			mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Member_months"] <- 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Member_months"] + 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 99110,"Member_months"]
			mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Premiums"] <- 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Premiums"] + 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 99110,"Premiums"]
			mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Claims"] <- 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Claims"] +
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 99110,"Claims"]
			mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"ACA_taxes_fees"] <- 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"ACA_taxes_fees"] +
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 99110,"ACA_taxes_fees"]
			mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"FDSTLCL_taxes_fees"] <- 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"FDSTLCL_taxes_fees"] +
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 99110,"FDSTLCL_taxes_fees"]
			mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Admin_costs"] <- 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Admin_costs"] + 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 99110,"Admin_costs"]
			mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Var_Admin_Low"] <- 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Var_Admin_Low"] +
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 99110,"Var_Admin_Low"]
			mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Var_Admin_High"] <- 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Var_Admin_High"] +
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 99110,"Var_Admin_High"]
			mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Fixed_Admin_Low"] <- 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Fixed_Admin_Low"] + 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 99110,"Fixed_Admin_Low"]
			mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Fixed_Admin_High"] <- 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Fixed_Admin_High"] + 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 99110,"Fixed_Admin_High"]
			mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"MLR_Rebate"] <- 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"MLR_Rebate"] + 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 99110,"MLR_Rebate"]
			mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Risk_adjustment_rec"] <- 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Risk_adjustment_rec"] + 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 99110,"Risk_adjustment_rec"]
			mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Reinsurance_rec"] <- 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Reinsurance_rec"] + 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 99110,"Reinsurance_rec"]
			mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Risk_corridor_rec"] <- 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Risk_corridor_rec"] + 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 99110,"Risk_corridor_rec"]
			mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Premiums_w3Rs"] <- 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 67138,"Premiums_w3Rs"] + 
				mlr_data[mlr_data$year == t & mlr_data$HIOS_ISSUER_ID == 99110,"Premiums_w3Rs"]
		}
		
		# Remove PPO Health Net
		mlr_data <- mlr_data[!mlr_data$HIOS_ISSUER_ID == 99110,]
		rownames(mlr_data) <- paste(mlr_data$GROUP_AFFILIATION,mlr_data$year,sep="_")	
		
	
##### Pricing, Cost, and Risk Adjustment factors
	
	data$age_cost_factor <- data$geog_cost_factor <- NA
	data[!is.na(data$AGE) & data$year <= 2017,"age_cost_factor"] <- 
		age_rating_factors[as.character(pmin(64,data[!is.na(data$AGE)  & data$year <= 2017,"AGE"])),
			"Rating_Factor"] 
	data[!is.na(data$AGE) & data$year > 2017,"age_cost_factor"] <- 
		age_rating_factors[as.character(pmin(64,data[!is.na(data$AGE)  & data$year > 2017,"AGE"])),
			"Rating_Factor2018"] 
	data[!is.na(data$plan_id),"geog_cost_factor"] <- plan_data[data[!is.na(data$plan_id),"plan_id"],"Plan_Geog_Factor"] 
	data$plan_factor <- plan_data[data$plan_id,"Pricing_Value"]
	data$cost_factor <- data$age_cost_factor * data$geog_cost_factor * data$plan_factor
	
	# Age/Smoking Cost Factor at Household Level
	households$age_tob_cost_factor <- by(data$age_cost_factor,data$household_year,sum)[rownames(households)] 
	gc()
	
	# Risk Adjustment Factor (AV and moral hazard factors from CMS paper on transfer formula)
	silver_plans <- c("Silver","Silver - Enhanced 73","Silver - Enhanced 87","Silver - Enhanced 94","Silver73","Silver87","Silver94")
	plan_data$ra_factor <- 0.6 * 1
	plan_data[plan_data$Metal_Level %in% c("Minimum Coverage","Catastrophic"),"ra_factor"] <- 0.57 * 1 
	plan_data[plan_data$Metal_Level %in% silver_plans,"ra_factor"] <- 0.7 * 1.03 
	plan_data[plan_data$Metal_Level %in% c("Gold"),"ra_factor"] <- 0.8 * 1.08 
	plan_data[plan_data$Metal_Level %in% c("Platinum"),"ra_factor"] <- 0.9 * 1.15 
	
	# Have to clean up the plan name variable in julia data object
	
		# Get rid of csr plans
		julia_data$plan_name <- as.character(julia_data$plan_name)
		
		julia_data[julia_data$plan_name %in% c("ANT_SIL73","ANT_SIL87","ANT_SIL94"),"plan_name"] <- "ANT_SIL"
		julia_data[julia_data$plan_name %in% c("ANT_SIL733","ANT_SIL873","ANT_SIL943"),"plan_name"] <- "ANT_SIL3"
		julia_data[julia_data$plan_name %in% c("BS_SIL73","BS_SIL87","BS_SIL94"),"plan_name"] <- "BS_SIL"
		julia_data[julia_data$plan_name %in% c("BS_SIL733","BS_SIL873","BS_SIL943"),"plan_name"] <- "BS_SIL3"
		julia_data[julia_data$plan_name %in% c("HN_SIL73","HN_SIL87","HN_SIL94"),"plan_name"] <- "HN_SIL"
		julia_data[julia_data$plan_name %in% c("HN_SIL733","HN_SIL873","HN_SIL943"),"plan_name"] <- "HN_SIL3"
		julia_data[julia_data$plan_name %in% c("KA_SIL73","KA_SIL87","KA_SIL94"),"plan_name"] <- "KA_SIL"
		julia_data[julia_data$plan_name %in% c("SMALL_SIL73","SMALL_SIL87","SMALL_SIL94"),"plan_name"] <- "SMALL_SIL"
		
	# Add region and year to julia_data
	julia_data$region <- households[julia_data$household_number,"rating_area"]
	julia_data$year <- households[julia_data$household_number,"year"]
	julia_data$plan_unique_id <- paste(julia_data$plan_name,julia_data$year,julia_data$region,sep="")
	
	# Need plan_name to refer to plan_data object
	
		julia_data$plan_name_ref <- julia_data$plan_name
	
		# Anthem
		julia_data[!julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "ANT_BR","plan_name_ref"] <- "ANT_BR2"
		julia_data[!julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "ANT_BR_HSA","plan_name_ref"] <- "ANT_BR_HSA2"
		julia_data[!julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "ANT_CAT","plan_name_ref"] <- "ANT_CAT2"
		julia_data[!julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "ANT_G","plan_name_ref"] <- "ANT_G2"
		julia_data[!julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "ANT_P","plan_name_ref"] <- "ANT_P2"
		julia_data[!julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "ANT_SIL","plan_name_ref"] <- "ANT_SIL2"
		
		# Blue Shield
		julia_data[!julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "BS_BR","plan_name_ref"] <- "BS_BR2"
		julia_data[!julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "BS_BR_HSA","plan_name_ref"] <- "BS_BR2_HSA"
		julia_data[!julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "BS_CAT","plan_name_ref"] <- "BS_CAT2"
		julia_data[!julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "BS_G","plan_name_ref"] <- "BS_G2"
		julia_data[!julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "BS_P","plan_name_ref"] <- "BS_P2"
		julia_data[!julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "BS_SIL","plan_name_ref"] <- "BS_SIL2"
		
		# Health Net
		epo_regions <- c(2,4,5,8,9,10)
		hsp_regions <- c(15,16,17,18,19)

		julia_data[julia_data$region %in% epo_regions & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_BR","plan_name_ref"] <- "HN_BR2"
		julia_data[julia_data$region %in% epo_regions & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_CAT","plan_name_ref"] <- "HN_CAT2"
		julia_data[julia_data$region %in% epo_regions & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_G","plan_name_ref"] <- "HN_G2"
		julia_data[julia_data$region %in% epo_regions & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_P","plan_name_ref"] <- "HN_P2"
		julia_data[julia_data$region %in% epo_regions & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_SIL","plan_name_ref"] <- "HN_SIL2"
		
		julia_data[julia_data$region %in% hsp_regions & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_BR","plan_name_ref"] <- "HN_BR4"
		julia_data[julia_data$region %in% hsp_regions & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_CAT","plan_name_ref"] <- "HN_CAT4"
		julia_data[julia_data$region %in% hsp_regions & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_G","plan_name_ref"] <- "HN_G4"
		julia_data[julia_data$region %in% hsp_regions & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_P","plan_name_ref"] <- "HN_P4"
		julia_data[julia_data$region %in% hsp_regions & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_SIL","plan_name_ref"] <- "HN_SIL4"
		
		julia_data[julia_data$region %in% c(7,14) & julia_data$year == 2015 & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_BR","plan_name_ref"] <- "HN_BR2"
		julia_data[julia_data$region %in% c(7,14) & julia_data$year == 2015 & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_CAT","plan_name_ref"] <- "HN_CAT2"
		julia_data[julia_data$region %in% c(7,14) & julia_data$year == 2015 & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_G","plan_name_ref"] <- "HN_G2"
		julia_data[julia_data$region %in% c(7,14) & julia_data$year == 2015 & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_P","plan_name_ref"] <- "HN_P2"
		julia_data[julia_data$region %in% c(7,14) & julia_data$year == 2015 & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_SIL","plan_name_ref"] <- "HN_SIL2"
		
		julia_data[julia_data$region %in% c(1,3,7,11) & julia_data$year %in% c(2016,2017) & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_BR","plan_name_ref"] <- "HN_BR4"
		julia_data[julia_data$region %in% c(1,3,7,11) & julia_data$year %in% c(2016,2017) & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_CAT","plan_name_ref"] <- "HN_CAT4"
		julia_data[julia_data$region %in% c(1,3,7,11) & julia_data$year %in% c(2016,2017) & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_G","plan_name_ref"] <- "HN_G4"
		julia_data[julia_data$region %in% c(1,3,7,11) & julia_data$year %in% c(2016,2017) & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_P","plan_name_ref"] <- "HN_P4"
		julia_data[julia_data$region %in% c(1,3,7,11) & julia_data$year %in% c(2016,2017) & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_SIL","plan_name_ref"] <- "HN_SIL4"
		
		julia_data[julia_data$region %in% c(14:19) & julia_data$year %in% c(2016:2019) & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_BR","plan_name_ref"] <- "HN_BR4"
		julia_data[julia_data$region %in% c(14:19) & julia_data$year %in% c(2016:2019) & !julia_data$plan_unique_id %in% rownames(plan_data) & julia_data$plan_name_ref == "HN_CAT","plan_name_ref"] <- "HN_CAT4"
		
		# Small Insurer
		
			plan_data$Plan_Name <- as.character(plan_data$Plan_Name)
			plan_data$Plan_Name_Small <- as.character(plan_data$Plan_Name_Small)
		
			for(n in c(1:19)) {
				for(t in c(2014:2019)) {
					
					# Catastrophic indices
					cat_indices <- which(plan_data$Plan_Name_Small == "SMALL_CAT" & plan_data$ENROLLMENT_YEAR == t & plan_data$region == n)
					if(length(cat_indices) > 0) {
						julia_data[julia_data$region == n & julia_data$year == t & julia_data$plan_name_ref == "SMALL_CAT","plan_name_ref"]  <- 
							plan_data[cat_indices[which.min(plan_data[cat_indices,"Premium"])],"Plan_Name"]
					}
					
					# Bronze indices
					bronze_indices <- which(plan_data$Plan_Name_Small == "SMALL_BR" & plan_data$ENROLLMENT_YEAR == t & plan_data$region == n)
					if(length(bronze_indices) > 0) {
						julia_data[julia_data$region == n & julia_data$year == t & julia_data$plan_name_ref == "SMALL_BR","plan_name_ref"]  <- 
							plan_data[bronze_indices[which.min(plan_data[bronze_indices,"Premium"])],"Plan_Name"]
					}
					
					# Bronze HSA indices
					bronze_hsa_indices <- which(plan_data$Plan_Name_Small == "SMALL_BR_HSA" & plan_data$ENROLLMENT_YEAR == t & plan_data$region == n)
					if(length(bronze_hsa_indices) > 0) {
						julia_data[julia_data$region == n & julia_data$year == t & julia_data$plan_name_ref == "SMALL_BR_HSA","plan_name_ref"]  <- 
							plan_data[bronze_hsa_indices[which.min(plan_data[bronze_hsa_indices,"Premium"])],"Plan_Name"]
					}
					
					# Silver indices
					silver_indices <- which(plan_data$Plan_Name_Small == "SMALL_SIL" & plan_data$ENROLLMENT_YEAR == t & plan_data$region == n)
					if(length(silver_indices) > 0) {
						julia_data[julia_data$region == n & julia_data$year == t & julia_data$plan_name_ref == "SMALL_SIL","plan_name_ref"]  <- 
							plan_data[silver_indices[which.min(plan_data[silver_indices,"Premium"])],"Plan_Name"]
					}
					
					# Gold indices
					gold_indices <- which(plan_data$Plan_Name_Small == "SMALL_G" & plan_data$ENROLLMENT_YEAR == t & plan_data$region == n)
					if(length(gold_indices) > 0) {
						julia_data[julia_data$region == n & julia_data$year == t & julia_data$plan_name_ref == "SMALL_G","plan_name_ref"]  <- 
							plan_data[gold_indices[which.min(plan_data[gold_indices,"Premium"])],"Plan_Name"]
					}
					
					# Platinum indices
					platinum_indices <- which(plan_data$Plan_Name_Small == "SMALL_P" & plan_data$ENROLLMENT_YEAR == t & plan_data$region == n)
					if(length(platinum_indices) > 0) {
						julia_data[julia_data$region == n & julia_data$year == t & julia_data$plan_name_ref == "SMALL_P","plan_name_ref"]  <- 
							plan_data[platinum_indices[which.min(plan_data[platinum_indices,"Premium"])],"Plan_Name"]
					}
				}
			}
		
		
		julia_data$plan_unique_id <- paste(julia_data$plan_name_ref,julia_data$year,julia_data$region,sep="")
		julia_data[julia_data$plan_name_ref == "Uninsured","plan_unique_id"] <- NA
		
	# Add rating factor to julia data
	julia_data$rating_factor <- households[julia_data$household_number,"rating_factor"]
	julia_data$age_factor <- julia_data$rating_factor
	
	# Add age cost factor to julia data
	julia_data$age_tob_cost_factor <- households[julia_data$household_number,"age_tob_cost_factor"]
		
	# Add pricing factor to julia data
	insured_indices <- which(!is.na(julia_data$plan_unique_id))
	julia_data$pricing_factor <- 0
	julia_data[insured_indices,"pricing_factor"] <- 
		plan_data[julia_data[insured_indices,"plan_unique_id"],"Pricing_Factor"] * julia_data[insured_indices,"rating_factor"]
	
	# Add age-geog factors to julia data
	julia_data$age_geog_factor <- 0
	julia_data[insured_indices,"age_geog_factor"] <- 
		plan_data[julia_data[insured_indices,"plan_unique_id"],"Plan_Geog_Factor"] * julia_data[insured_indices,"rating_factor"]
	
	# Add age-geog cost factors to julia data
	julia_data$age_geog_cost_factor <- 0
	julia_data[insured_indices,"age_geog_cost_factor"] <- 
		plan_data[julia_data[insured_indices,"plan_unique_id"],"Plan_Geog_Factor"] * julia_data[insured_indices,"age_tob_cost_factor"]
	
	# Add cost factors to julia data
	julia_data$cost_factor <- 0
	julia_data[insured_indices,"cost_factor"] <- 
		plan_data[julia_data[insured_indices,"plan_unique_id"],"Pricing_Factor"] * julia_data[insured_indices,"age_tob_cost_factor"]
	
	# Add RA factors to julia data
	julia_data$ra_factor <- 0
	julia_data[insured_indices,"ra_factor"] <- 
		plan_data[julia_data[insured_indices,"plan_unique_id"],"ra_factor"]
	

#### Let's prepare the commissions object	
	
	big_insurers <- c("Anthem","Blue Shield","Health Net","Kaiser")
	
	# Consolidate the commissions:
		# 2014: 5 commission variables
		# 2015: 9 commission variables
		# 2016: 9 commission variables
		# 2017: 9 commission variables
		# 2018: 9 commission variables
		# NOTE: This should be computationally feasible (25-30 premium variables)
		
	commissions_data$Insurer <- as.character(commissions_data$Insurer)
	commissions_data$Network <- as.character(commissions_data$Network)
	commissions_data$Type <- as.character(commissions_data$Type)
	
	# 2014
		
		add_columns <- c("Insurer","Network","Type","Y2014")
		to_add <- commissions_data[!is.na(commissions_data$Y2014),add_columns]
		to_add[!to_add$Insurer %in% big_insurers,"Insurer"] <- "Small_Insurer"
		colnames(to_add)[which(colnames(to_add) == "Y2014")] <- "Commission"
		
		# Delete any rows for a renewal (everyone is new in 2014)
		to_add <- to_add[to_add$Type == "New",]
		
		# Combine all Health Net rows (same commission)
		to_add <- to_add[!to_add$Insurer == "Health Net" | (!duplicated(to_add$Insurer) & to_add$Insurer == "Health Net"),]
		to_add[to_add$Insurer == "Health Net",c("Network","Type")] <- "" 
		
		# Average all Small Insurers with flat commission (all except Sharp)
		small_average_comm <- mean(to_add[to_add$Insurer == "Small_Insurer" & to_add$Commission > 1,"Commission"])
		to_add <- to_add[to_add$Insurer %in% big_insurers | (!duplicated(to_add$Insurer) & to_add$Insurer == "Small_Insurer"),]
		to_add[to_add$Insurer == "Small_Insurer","Commission"] <- small_average_comm
		
		to_add[,c("Network","Type")] <- "" 
		
		to_add$Year <- 2014
		commissions <- to_add
		
	# 2015	
		
		add_columns <- c("Insurer","Network","Type","Y2015")
		to_add <- commissions_data[!is.na(commissions_data$Y2015),add_columns]
		to_add[!to_add$Insurer %in% big_insurers,"Insurer"] <- "Small_Insurer"
		colnames(to_add)[which(colnames(to_add) == "Y2015")] <- "Commission"
		
		# Combine all Health Net rows PPO New and PPO Renewal, HMO New and HMO Renewal (same commission)
		to_add <- to_add[setdiff(1:nrow(to_add),
			which(to_add$Insurer == "Health Net" & to_add$Network == "PPO" & to_add$Type == "Renewal")),]
		to_add <- to_add[setdiff(1:nrow(to_add),
			which(to_add$Insurer == "Health Net" & to_add$Network == "HMO" & to_add$Type == "Renewal")),]
		to_add[to_add$Insurer == "Health Net","Type"] <- ""
		
		# Average all Small Insurers with flat commission (all except Sharp)
		small_average_comm <- mean(to_add[to_add$Insurer == "Small_Insurer" & to_add$Commission > 1,"Commission"])
		to_add <- to_add[to_add$Insurer %in% big_insurers | (!duplicated(to_add$Insurer) & to_add$Insurer == "Small_Insurer"),]
		to_add[to_add$Insurer == "Small_Insurer","Commission"] <- small_average_comm
		to_add[to_add$Insurer == "Small_Insurer",c("Network","Type")] <- "" 
		
		# Blue Shield - only sold PPO plans
		to_add[to_add$Insurer == "Blue Shield","Network"] <- ""
		
		to_add$Year <- 2015
		commissions <- rbind(commissions,to_add)
		
	# 2016	
		
		add_columns <- c("Insurer","Network","Type","Y2016")
		to_add <- commissions_data[!is.na(commissions_data$Y2016),add_columns]
		to_add[!to_add$Insurer %in% big_insurers,"Insurer"] <- "Small_Insurer"
		colnames(to_add)[which(colnames(to_add) == "Y2016")] <- "Commission"
		
		# Combine all Health Net rows PPO New and PPO Renewal, HMO New and HMO Renewal (same commission)
		to_add <- to_add[setdiff(1:nrow(to_add),
			which(to_add$Insurer == "Health Net" & to_add$Network == "PPO" & to_add$Type == "Renewal")),]
		to_add <- to_add[setdiff(1:nrow(to_add),
			which(to_add$Insurer == "Health Net" & to_add$Network == "HMO" & to_add$Type == "Renewal")),]
		to_add[to_add$Insurer == "Health Net","Type"] <- ""
		
		# Average all Small Insurers with flat commission (all except Sharp)
		small_average_comm <- mean(to_add[to_add$Insurer == "Small_Insurer" & to_add$Commission > 1,"Commission"])
		to_add <- to_add[to_add$Insurer %in% big_insurers | (!duplicated(to_add$Insurer) & to_add$Insurer == "Small_Insurer"),]
		to_add[to_add$Insurer == "Small_Insurer","Commission"] <- small_average_comm
		to_add[to_add$Insurer == "Small_Insurer",c("Network","Type")] <- "" 
		
		# Blue Shield - only sold PPO plans
		to_add[to_add$Insurer == "Blue Shield","Network"] <- ""
		
		to_add$Year <- 2016
		commissions <- rbind(commissions,to_add)
		
	# 2017	
		
		add_columns <- c("Insurer","Network","Type","Y2017")
		to_add <- commissions_data[!is.na(commissions_data$Y2017),add_columns]
		to_add[!to_add$Insurer %in% big_insurers,"Insurer"] <- "Small_Insurer"
		colnames(to_add)[which(colnames(to_add) == "Y2017")] <- "Commission"
		
		# Combine Blue Shield PPO New and HMO New, PPO Renewal and HMO Renewal (same commission)
		to_add <- to_add[setdiff(1:nrow(to_add),
			which(to_add$Insurer == "Blue Shield" & to_add$Network == "HMO" & to_add$Type == "New")),]
		to_add <- to_add[setdiff(1:nrow(to_add),
			which(to_add$Insurer == "Blue Shield" & to_add$Network == "HMO" & to_add$Type == "Renewal")),]
		to_add[to_add$Insurer == "Blue Shield","Network"] <- ""
		
		# Combine all Health Net rows PPO New and PPO Renewal, HMO New and HMO Renewal (same commission)
		to_add <- to_add[setdiff(1:nrow(to_add),
			which(to_add$Insurer == "Health Net" & to_add$Network == "PPO" & to_add$Type == "Renewal")),]
		to_add <- to_add[setdiff(1:nrow(to_add),
			which(to_add$Insurer == "Health Net" & to_add$Network == "HMO" & to_add$Type == "Renewal")),]
		to_add[to_add$Insurer == "Health Net","Type"] <- ""
			
		# Average all Small Insurers with flat commission (all except Sharp)
		small_average_comm <- mean(to_add[to_add$Insurer == "Small_Insurer" & to_add$Commission > 1,"Commission"])
		to_add <- to_add[to_add$Insurer %in% big_insurers | (!duplicated(to_add$Insurer) & to_add$Insurer == "Small_Insurer"),]
		to_add[to_add$Insurer == "Small_Insurer","Commission"] <- small_average_comm
		to_add[to_add$Insurer == "Small_Insurer",c("Network","Type")] <- "" 
		
		to_add$Year <- 2017
		commissions <- rbind(commissions,to_add)
			
	# 2018	
		
		add_columns <- c("Insurer","Network","Type","Y2018")
		to_add <- commissions_data[!is.na(commissions_data$Y2018),add_columns]
		to_add[!to_add$Insurer %in% big_insurers,"Insurer"] <- "Small_Insurer"
		colnames(to_add)[which(colnames(to_add) == "Y2018")] <- "Commission"
		
		# Combine Anthem (same commission)
		to_add <- to_add[!(to_add$Insurer == "Anthem") | (!duplicated(to_add$Insurer) & to_add$Insurer == "Anthem"),] 
		to_add[to_add$Insurer == "Anthem",c("Network","Type")] <- "" 
		
		# Combine all Health Net rows (same commission)
		to_add <- to_add[!to_add$Insurer == "Health Net" | (!duplicated(to_add$Insurer) & to_add$Insurer == "Health Net"),]
		to_add[to_add$Insurer == "Health Net",c("Network","Type")] <- "" 
			
		# Average all Small Insurers with flat commission (all except Sharp)
		small_average_comm <- mean(to_add[to_add$Insurer == "Small_Insurer" & to_add$Commission > 1,"Commission"])
		to_add <- to_add[to_add$Insurer %in% big_insurers | (!duplicated(to_add$Insurer) & to_add$Insurer == "Small_Insurer"),]
		to_add[to_add$Insurer == "Small_Insurer","Commission"] <- small_average_comm
		to_add[to_add$Insurer == "Small_Insurer",c("Network","Type")] <- "" 
		
		to_add$Year <- 2018
		commissions <- rbind(commissions,to_add)
		commissions$ID <- paste(commissions$Insurer,commissions$Network,commissions$Type,commissions$Year,sep="") 
		rownames(commissions) <- commissions$ID

	# Now we need to be able to reference the commissions object from julia_data
	
		julia_data_insurers <- plans[julia_data$plan_name,"Insurer"]
		julia_data_networks <- rep("PPO",nrow(julia_data))
		julia_data_networks[plans[julia_data$plan_name,"HMO"] == 1] <- "HMO"
	
		julia_data_renewals <- rep("New",nrow(julia_data))
		julia_data_renewals[julia_data$renewal == 1] <- "Renewal"
		julia_data_renewals[julia_data$renewal == -1] <- NA
		
		julia_data$Commission_ID <- NA
	
		# 2014
		julia_data[julia_data_insurers == "Anthem" & !is.na(julia_data_insurers) & julia_data$year == 2014,"Commission_ID"]	 <- "Anthem2014"
		julia_data[julia_data_insurers == "Blue_Shield" & !is.na(julia_data_insurers) & julia_data$year == 2014,"Commission_ID"]	 <- "Blue Shield2014"
		julia_data[julia_data_insurers == "Health_Net" & !is.na(julia_data_insurers) & julia_data$year == 2014,"Commission_ID"]	 <- "Health Net2014" 
		julia_data[julia_data_insurers == "Kaiser" & !is.na(julia_data_insurers) & julia_data$year == 2014,"Commission_ID"]	 <- "Kaiser2014"  
		julia_data[julia_data_insurers == "Small_Insurer" & !is.na(julia_data_insurers) & julia_data$year == 2014,"Commission_ID"]	 <- "Small_Insurer2014"
			
		# 2015
		julia_data[julia_data_insurers == "Anthem" & !is.na(julia_data_insurers) & julia_data$year == 2015 & julia_data$renewal == 0,"Commission_ID"]	 <- "AnthemNew2015"
		julia_data[julia_data_insurers == "Anthem" & !is.na(julia_data_insurers) & julia_data$year == 2015 & julia_data$renewal == 1,"Commission_ID"]	 <- "AnthemRenewal2015"
		julia_data[julia_data_insurers == "Blue_Shield" & !is.na(julia_data_insurers) & julia_data$year == 2015 & julia_data$renewal == 0,"Commission_ID"]	 <- "Blue ShieldNew2015"
		julia_data[julia_data_insurers == "Blue_Shield" & !is.na(julia_data_insurers) & julia_data$year == 2015 & julia_data$renewal == 1,"Commission_ID"]	 <- "Blue ShieldRenewal2015"
		julia_data[julia_data_insurers == "Health_Net" & !is.na(julia_data_insurers) & julia_data$year == 2015 & julia_data_networks == "PPO","Commission_ID"]	 <- "Health NetPPO2015"
		julia_data[julia_data_insurers == "Health_Net" & !is.na(julia_data_insurers) & julia_data$year == 2015 & julia_data_networks == "HMO","Commission_ID"]	 <- "Health NetHMO2015"
		julia_data[julia_data_insurers == "Kaiser" & !is.na(julia_data_insurers) & julia_data$year == 2015 & julia_data$renewal == 0,"Commission_ID"]	 <- "KaiserNew2015"
		julia_data[julia_data_insurers == "Kaiser" & !is.na(julia_data_insurers) & julia_data$year == 2015 & julia_data$renewal == 1,"Commission_ID"]	 <- "KaiserRenewal2015"
		julia_data[julia_data_insurers == "Small_Insurer" & !is.na(julia_data_insurers) & julia_data$year == 2015,"Commission_ID"]	 <- "Small_Insurer2015"
		
		# 2016
		julia_data[julia_data_insurers == "Anthem" & !is.na(julia_data_insurers) & julia_data$year == 2016 & julia_data$renewal == 0,"Commission_ID"]	 <- "AnthemNew2016"
		julia_data[julia_data_insurers == "Anthem" & !is.na(julia_data_insurers) & julia_data$year == 2016 & julia_data$renewal == 1,"Commission_ID"]	 <- "AnthemRenewal2016"
		julia_data[julia_data_insurers == "Blue_Shield" & !is.na(julia_data_insurers) & julia_data$year == 2016 & julia_data$renewal == 0,"Commission_ID"]	 <- "Blue ShieldNew2016"
		julia_data[julia_data_insurers == "Blue_Shield" & !is.na(julia_data_insurers) & julia_data$year == 2016 & julia_data$renewal == 1,"Commission_ID"]	 <- "Blue ShieldRenewal2016"
		julia_data[julia_data_insurers == "Health_Net" & !is.na(julia_data_insurers) & julia_data$year == 2016 & julia_data_networks == "PPO","Commission_ID"]	 <- "Health NetPPO2016"
		julia_data[julia_data_insurers == "Health_Net" & !is.na(julia_data_insurers) & julia_data$year == 2016 & julia_data_networks == "HMO","Commission_ID"]	 <- "Health NetHMO2016"
		julia_data[julia_data_insurers == "Kaiser" & !is.na(julia_data_insurers) & julia_data$year == 2016 & julia_data$renewal == 0,"Commission_ID"]	 <- "KaiserNew2016"
		julia_data[julia_data_insurers == "Kaiser" & !is.na(julia_data_insurers) & julia_data$year == 2016 & julia_data$renewal == 1,"Commission_ID"]	 <- "KaiserRenewal2016"
		julia_data[julia_data_insurers == "Small_Insurer" & !is.na(julia_data_insurers) & julia_data$year == 2016,"Commission_ID"]	 <- "Small_Insurer2016"
		
		# 2017
		julia_data[julia_data_insurers == "Anthem" & !is.na(julia_data_insurers) & julia_data$year == 2017 & julia_data$renewal == 0,"Commission_ID"]	 <- "AnthemNew2017"
		julia_data[julia_data_insurers == "Anthem" & !is.na(julia_data_insurers) & julia_data$year == 2017 & julia_data$renewal == 1,"Commission_ID"]	 <- "AnthemRenewal2017"
		julia_data[julia_data_insurers == "Blue_Shield" & !is.na(julia_data_insurers) & julia_data$year == 2017 & julia_data$renewal == 0,"Commission_ID"]	 <- "Blue ShieldNew2017"
		julia_data[julia_data_insurers == "Blue_Shield" & !is.na(julia_data_insurers) & julia_data$year == 2017 & julia_data$renewal == 1,"Commission_ID"]	 <- "Blue ShieldRenewal2017"
		julia_data[julia_data_insurers == "Health_Net" & !is.na(julia_data_insurers) & julia_data$year == 2017 & julia_data_networks == "PPO","Commission_ID"]	 <- "Health NetPPO2017"
		julia_data[julia_data_insurers == "Health_Net" & !is.na(julia_data_insurers) & julia_data$year == 2017 & julia_data_networks == "HMO","Commission_ID"]	 <- "Health NetHMO2017"
		julia_data[julia_data_insurers == "Kaiser" & !is.na(julia_data_insurers) & julia_data$year == 2017 & julia_data$renewal == 0,"Commission_ID"]	 <- "KaiserNew2017"
		julia_data[julia_data_insurers == "Kaiser" & !is.na(julia_data_insurers) & julia_data$year == 2017 & julia_data$renewal == 1,"Commission_ID"]	 <- "KaiserRenewal2017"
		julia_data[julia_data_insurers == "Small_Insurer" & !is.na(julia_data_insurers) & julia_data$year == 2017,"Commission_ID"]	 <- "Small_Insurer2017"
		
		# 2018
		julia_data[julia_data_insurers == "Anthem" & !is.na(julia_data_insurers) & julia_data$year == 2018,"Commission_ID"]	 <- "Anthem2018"
		julia_data[julia_data_insurers == "Blue_Shield" & !is.na(julia_data_insurers) & julia_data$year == 2018 & julia_data_networks == "PPO" & julia_data$renewal == 0,"Commission_ID"]	 <-  "Blue ShieldPPONew2018"
		julia_data[julia_data_insurers == "Blue_Shield" & !is.na(julia_data_insurers) & julia_data$year == 2018 & julia_data_networks == "PPO" & julia_data$renewal == 1,"Commission_ID"]	 <-  "Blue ShieldPPORenewal2018"
		julia_data[julia_data_insurers == "Blue_Shield" & !is.na(julia_data_insurers) & julia_data$year == 2018 & julia_data_networks == "HMO" & julia_data$renewal == 0,"Commission_ID"]	 <-  "Blue ShieldHMONew2018"
		julia_data[julia_data_insurers == "Blue_Shield" & !is.na(julia_data_insurers) & julia_data$year == 2018 & julia_data_networks == "HMO" & julia_data$renewal == 1,"Commission_ID"]	 <-  "Blue ShieldHMORenewal2018"
		julia_data[julia_data_insurers == "Health_Net" & !is.na(julia_data_insurers) & julia_data$year == 2018,"Commission_ID"]	 <- "Health Net2018"
		julia_data[julia_data_insurers == "Kaiser" & !is.na(julia_data_insurers) & julia_data$year == 2018 & julia_data$renewal == 0,"Commission_ID"]	 <- "KaiserNew2018"
		julia_data[julia_data_insurers == "Kaiser" & !is.na(julia_data_insurers) & julia_data$year == 2018 & julia_data$renewal == 1,"Commission_ID"]	 <- "KaiserRenewal2018"
		julia_data[julia_data_insurers == "Small_Insurer" & !is.na(julia_data_insurers) & julia_data$year == 2018,"Commission_ID"]	 <- "Small_Insurer2018"
		
	# Let's check how well we matched the commissions variable in julia data
	
		# A few oddball cases were labeled new enrollees when they should have been renewals
			# EX: somebody ages out of catastrophic
			# EX: somebody's previous plan is no longer offered 
		
		com <- rep(0,nrow(julia_data))
		com[!is.na(julia_data$Commission_ID)] <- 
			commissions[julia_data[!is.na(julia_data$Commission_ID),"Commission_ID"] ,"Commission"] 
		
		bs_indices <- which(julia_data_insurers == "Blue_Shield" & !is.na(julia_data_insurers))
		com[bs_indices] <- com[bs_indices] * 
			(julia_data[bs_indices,"premium"] + julia_data[bs_indices,"subsidy"] + julia_data[bs_indices,"penalty"])/
			households[julia_data[bs_indices,"household_number"],"enrollees"]
			
		hn_indices <- which(julia_data_insurers == "Health_Net" & !is.na(julia_data_insurers) & julia_data$year <= 2017)	
		com[hn_indices] <- com[hn_indices] * 
			(julia_data[hn_indices,"premium"] + julia_data[hn_indices,"subsidy"] + julia_data[hn_indices,"penalty"])/
			households[julia_data[hn_indices,"household_number"],"enrollees"]
		
		# Check
			# The big insurers match perfectly
			# The small insurers have quite a few discrepancies, but that's to be expected because we averaged
		#z <- julia_data[abs(julia_data$commission - com) > 0.0001 & julia_data$year <= 2018,]

		# * households[julia_data$household_number,"broker"]
		
	# Recode insurer variable
	commissions[commissions$Insurer == "Blue Shield","Insurer"] <- "Blue_Shield"
	commissions[commissions$Insurer == "Health Net","Insurer"] <- "Health_Net"
	
	# Remove NAs from Type field
	commissions[commissions$Type == "","Type"] <- "Both"
		
	# Remove NAs from Type field
	commissions[commissions$Network == "","Network"] <- "Both"
	
#### Add base plan premiums to plans object

	plan_data$insurer_small <- plan_data$Insurer
	plan_data[!plan_data$insurer_small %in% insurers,"insurer_small"] <- "Small_Insurer"
		
	plans$Premium_2014 <- plans$Premium_2015 <- plans$Premium_2016 <-
		plans$Premium_2017 <- plans$Premium_2018 <- plans$Premium_2019 <- 0
	
	base_plan_region <- 15
	
	for(t in c(2014:2019)) {
		
		col <- paste("Premium",t,sep="_")
		
		for(f in insurers) {
			plan_indices <- which(plan_data$ENROLLMENT_YEAR == t & 
				plan_data$region == base_plan_region & plan_data$insurer_small == f)
			base_plan_index <- intersect(plan_indices,which(plan_data$Base_Plan == 1))	
			
			# Toss CSR plans
			plan_indices <- setdiff(plan_indices,which(plan_data$metal_level %in% 
				c("Silver - Enhanced 73","Silver - Enhanced 87","Silver - Enhanced 94")))
			
			# Toss HSA plans
			plan_indices <- setdiff(plan_indices,which(plan_data$HSA == 1))
			
			# Toss Catastrophic Plans
			plan_indices <- setdiff(plan_indices,which(plan_data$Metal_Level == "Minimum Coverage"))
			
			# Check if any duplicates - you'll definitely have duplicates for Small insurer, possibly Health Net
			duplicates <- plan_indices[which(duplicated(plan_data[plan_indices,"Plan_Name_Small_NOHSACAT"]))]
			if(length(duplicates) > 0) {
				duplicate_plan_names <- unique(plan_data[duplicates,"Plan_Name_Small_NOHSACAT"])
				for(j in duplicate_plan_names) {
					all_duplicate_indices <- intersect(plan_indices,which(plan_data$Plan_Name_Small_NOHSACAT == j))
					lowest_prem_index <- all_duplicate_indices[which.min(plan_data[all_duplicate_indices,"Premium"])]
					
					# Remove all but the lowest_prem_index
					plan_indices <- setdiff(plan_indices,setdiff(all_duplicate_indices,lowest_prem_index))
				}
			}
			
			# Insert Premium into plans object
			plans[plan_data[plan_indices,"Plan_Name_Small_NOHSACAT"],col] <- plan_data[plan_indices,"Premium"]
			
		}
	}
	
	# A few issues with the above
	
		# Anthem not in 15 in 2018-2019 (use RA7 as base)
		for(t in c(2018:2019)) {
			col <- paste("Premium",t,sep="_")
			plan_indices <- which(plan_data$ENROLLMENT_YEAR == t & 
					plan_data$region == 7 & plan_data$insurer_small == "Anthem")
			base_plan_index <- intersect(plan_indices,which(plan_data$Base_Plan == 1))	
			
			# Toss CSR plans
			csr_plans <- intersect(plan_indices,which(plan_data$metal_level %in% 
				c("Silver - Enhanced 73","Silver - Enhanced 87","Silver - Enhanced 94")))
			plan_indices <- setdiff(plan_indices,csr_plans)
			
			# Insert Premium into plans object
			plans[plan_data[plan_indices,"Plan_Name_Small_NOHSACAT"],col] <- plan_data[plan_indices,"Premium"]
		}		
	
	
		# Health PPO (HN_SIL,HN_G,HN_P) not in 15 in 2014-2017
		for(t in c(2014:2017)) {
		
			col <- paste("Premium",t,sep="_")
			HN_base_premium <- plan_data[which(plan_data$ENROLLMENT_YEAR == t & 
					plan_data$Base_Plan == 1 & plan_data$insurer_small == "Health_Net"),"Premium"]
			
			for(j in c("HN_SIL","HN_G","HN_P")) {
				
				# Pull pricing value from one rating area and multiply by base premium (pricing values are all the same)
				
				plans[j,col] <- 
					plan_data[plan_data$ENROLLMENT_YEAR == t & plan_data$region == 2 & 
					plan_data$insurer_small == "Health_Net" & plan_data$Plan_Name_Small == j,"Pricing_Value"] * HN_base_premium
			}
		}
		
		# Health Net PPO Bronze not in 15 in 2015
		for(t in c(2015)) {
		
			col <- paste("Premium",t,sep="_")
			HN_base_premium <- plan_data[which(plan_data$ENROLLMENT_YEAR == t & 
					plan_data$Base_Plan == 1 & plan_data$insurer_small == "Health_Net"),"Premium"]
			
			for(j in c("HN_BR")) {
				
				# Pull pricing value from one rating area and multiply by base premium (pricing values are all the same)
				
				plans[j,col] <- 
					plan_data[plan_data$ENROLLMENT_YEAR == t & plan_data$region == 2 & 
					plan_data$insurer_small == "Health_Net" & plan_data$Plan_Name_Small == j,"Pricing_Value"] * HN_base_premium
			}
		}
		
		
	# Remove unneeded columns
	plans$Plan_Name_NOCSR <- NULL
	plans$MSP <- NULL
	plans$Premium <- NULL
	plans$Catastrophic <- NULL
	plans$Bronze <- NULL
	plans$Gold <- NULL
	plans$Platinum <- NULL
	
#### Create plans_pt object

	plans_pt <- rbind(plans,plans,plans,plans,plans,plans)
	plans_pt$year <- c(rep(2014,nrow(plans)),rep(2015,nrow(plans)),rep(2016,nrow(plans)),
						rep(2017,nrow(plans)),rep(2018,nrow(plans)),rep(2019,nrow(plans)))
	plans_pt$Premium <- c(plans$Premium_2014,plans$Premium_2015,plans$Premium_2016,
							plans$Premium_2017,plans$Premium_2018,plans$Premium_2019)
	plans_pt$plan_name <- plans$Plan_Name
	plans_pt$pt_name <- paste(plans_pt$plan_name,plans_pt$year,sep="")
	
	plans_pt$reins_factor <- rate_data[paste(plans_pt$Insurer,plans_pt$year,sep="_"),"RI"]/rate_data[paste(plans_pt$Insurer,plans_pt$year,sep="_"),"Claims"]			
	plans_pt[is.na(plans_pt$reins_factor),"reins_factor"] <- 0		
	
	plans_pt$variable_admin <- pmin(mlr_data[paste(plans_pt$Insurer,plans_pt$year,sep="_"),"Var_Admin_Low"]/
		mlr_data[paste(plans_pt$Insurer,plans_pt$year,sep="_"),"Member_months"],65)	
	plans_pt[is.na(plans_pt$variable_admin),"variable_admin"] <- 0		
	
	plans_pt$fixed_admin <- mlr_data[paste(plans_pt$Insurer,plans_pt$year,sep="_"),"Fixed_Admin_Low"]/
		mlr_data[paste(plans_pt$Insurer,plans_pt$year,sep="_"),"Member_months"]
	plans_pt[is.na(plans_pt$fixed_admin),"fixed_admin"] <- 0					
	
	keep_fields <- c("pt_name","plan_name","year","Metal_Level","AV","Silver","HMO","Insurer",
		"Anthem","Blue_Shield","Kaiser","Health_Net","Premium","reins_factor","variable_admin","fixed_admin")
	plans_pt <- plans_pt[,keep_fields]
	plans_pt <- plans_pt[plans_pt$Premium != 0,]
	
#### Create plans_pmt object
	# Fields: pmt_name, plan_name, year, rating_area, AV, HMO, Anthem, Blue_Shield, Kaiser, Health_Net
	
	keep_fields <- c("pmt_name","Plan_Name_Small_NOHSACAT","ENROLLMENT_YEAR","region","Metal_Level","PLAN_NETWORK_TYPE","insurer_small")
	plan_data$pmt_name <- paste(plan_data$Plan_Name_Small_NOHSACAT,plan_data$ENROLLMENT_YEAR,plan_data$region,sep="")
	plans_pmt <- plan_data[!duplicated(plan_data$pmt_name),keep_fields]
	
	plans_pmt$plan_name <- plans_pmt$Plan_Name_Small_NOHSACAT
	plans_pmt$year <- plans_pmt$ENROLLMENT_YEAR
	plans_pmt$rating_area <- plans_pmt$region
	plans_pmt$HMO <- as.numeric(plans_pmt$PLAN_NETWORK_TYPE == "HMO")
	plans_pmt$Anthem <- as.numeric(plans_pmt$insurer_small == "Anthem")
	plans_pmt$Blue_Shield <- as.numeric(plans_pmt$insurer_small == "Blue_Shield")
	plans_pmt$Health_Net <- as.numeric(plans_pmt$insurer_small == "Health_Net")
	plans_pmt$Kaiser <- as.numeric(plans_pmt$insurer_small == "Kaiser")
	plans_pmt$Insurer <- plans_pmt$insurer_small
	
	plans_pmt$AV <- 0.6
	plans_pmt[plans_pmt$Metal_Level == "Silver","AV"] <- 0.7
	plans_pmt[plans_pmt$Metal_Level == "Gold","AV"] <- 0.8
	plans_pmt[plans_pmt$Metal_Level == "Platinum","AV"] <- 0.9
	
	# Add proj_name and rate_name
	rsdata <- read.csv("data/final/moments_data.csv",header=TRUE,row.names=1)
	plans_pmt$rate_name <- plans_pmt$pmt_name	
	plans_pmt[!plans_pmt$rate_name %in% rsdata$pmt_name,"rate_name"]	<- 
		paste(plans_pmt[!plans_pmt$rate_name %in% rsdata$pmt_name,"plan_name"],plans_pmt[!plans_pmt$rate_name %in% rsdata$pmt_name,"year"],0,sep="")
	
		# Fix remaining discrepancy
		plans_pmt[plans_pmt$rate_name == "BS_G320170","rate_name"] <- "BS_G320174" # B/c we combined regions 2 and 4
		
	plans_pmt$proj_name <- rsdata[plans_pmt$rate_name,"proj_name"]
	
	keep_fields <- c("pmt_name","proj_name","rate_name","plan_name","year","rating_area","AV","HMO",
		"Anthem","Blue_Shield","Kaiser","Health_Net","Insurer")
	plans_pmt <- plans_pmt[,keep_fields]
	
#### Check rating factor in julia data object

	# Let's make sure the rating factor (actually I use age_geog_factor) is correct
	rownames(plans_pt) <- plans_pt$pt_name
	julia_data$pt_name <- paste(julia_data$plan_name,julia_data$year,sep="")
	
	julia_data$base_premium <- NA
	julia_data[julia_data$AV > 0,"base_premium"] <- plans_pt[julia_data[julia_data$AV > 0,"pt_name"],"Premium"]  
	
	julia_data$calculated_premium <- julia_data$premium + julia_data$subsidy + julia_data$penalty
	
	julia_data$age_geog_factor <- julia_data$calculated_premium/julia_data$base_premium
	julia_data[is.na(julia_data$age_geog_factor),"age_geog_factor"] <- 0
	
#### Format output

	keep_columns <- c("plan_name","household_number","choice","previous_choice","new_plan","premium","subsidy","penalty","year",
		"weight","uninsured_plan","AV","pricing_factor","age_geog_factor","age_factor","cost_factor","ra_factor",
		"commission","renewal","Commission_ID")
	
	write.csv(julia_data[,keep_columns],file="ca_julia_supply_data_DEC312020_nav.csv",row.names=FALSE)
	write.csv(plans_pt,file="ca_plan_year_DEC312020_nav.csv",row.names=FALSE)	
	write.csv(plans_pmt,file="ca_plan_market_year_DEC312020_nav.csv",row.names=FALSE)
	write.csv(commissions,file="ca_commissions_DEC312020_nav.csv",row.names=FALSE)
	gc()
	