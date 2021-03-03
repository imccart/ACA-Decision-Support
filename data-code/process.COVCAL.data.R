# Load Data
data <- read.csv("data/pra_07192019.csv") # Covered California enrollment data
plan_data <- read.csv("data/plan_data.csv") # Covered California plan data
product_definitions <- read.csv("data/product_definitions.csv",row.names = 1) # definitions of column names in zip3_choices
zip3_choices <- read.csv("data/zip3_choices.csv",row.names = 1) # choice set by 3 digit zip and rating area
age_rating_factors <- read.csv("data/age_rating_factors.csv",row.names=1) # CCIIO default rating curve
poverty_guidelines <- read.csv("data/poverty_guidelines.csv",row.names=1) # Poverty guidelines
contribution_percentages <- read.csv("data/contribution_percentages.csv",row.names = 1) # ACA contribution percentages
rating_areas <- read.csv("data/rating_areas.csv",row.names = 1) # California county-rating area mapping


set.seed(6)

#### Data Cleanup

	### Strategy:
		# Group 1: Assume that the following variables are accurately reported:
			# ISSUER_NAME
			# PLAN_NETWORK_TYPE
			# ENROLLMENT_YEAR
		# Group 2: These variables are mostly correct
			# PLAN_LEVEL: 11 missing values (fill in 7 of 11 from metal_level_enhanced)
			# metal_level_enhanced: 4 missing values, need to do sanity checks within households
			# region: 21 missing values (fill in most from RES_COUNTY)
			# GENDER: 198 missing values (probably won't be able to fill in)
		# Group 3: These variables are somewhat problematic:
			# GROSS_PREMIUM_AMT: 11 missing values, needs to be consistent with age of household members
			# NET_PREMIUM_AMT: 11  missing values, should not be less than zero, needs to be consistent with income
		# Group 4: These variables have a lot of issues:
			# RES_COUNTY: 3,186 missing values, need to make sure consistent with region
			# AGE_BRACKET: 544 missing values, need to make sure consistent with GROSS_PREMIUM_AMT
			# subsidy_fpl_bracket: about half a million missing, need to make sure consistent with NET_PREMIUM_AMT and metal_level_enhanced
			# subsidy_eligible: 3,625 missing values, should be consistent with GROSS - NET premium, metal_level_enhanced, etc.

		# Fix as many variables as possible before repairing household construction
		# Then fix the rest during the household construction
			
		# These variables should be the same across the household:
			# RES_COUNTY and region
			# GROSS_PREMIUM_AMT and NET_PREMIUM_AMT (unless subsidized and unsubsidized members in household)
			# subsidy_fpl_bracket
			# Sanity check: metal level		
			
	### Flag for record with major problems

		data$flagged <- FALSE
			
	# Rename variables
		
		data$year <- data$enrlee_enrlmnt_yr
		data$individual_id <- data$indv_id_x
		data$insurer <- as.character(data$issuer_name)

		data$enrlee_enrlmnt_yr <- NULL
		data$indv_id_x <- NULL
		data$issuer_name <- NULL

			
	### Create/Repair key variables
		
		# Gender (Male = 1, Female = 0)
		data$gender <- as.character(data$gender)
		data[data$gender == "Male","gender"] <- 1
		data[data$gender == "Female","gender"] <- 0
		data[data$gender == "","flagged"] <- TRUE
		data[data$gender == "","gender"] <- NA
				
		# Metal - fill in missing values
		
		data$metal_level_enhanced <- as.character(data$metal_level_enhanced)
		missing_metals <- which(data$metal_level_enhanced == "")
		missing_bronze <- intersect(missing_metals,grep("Bronze",data$plan_name))
		missing_silver <- intersect(missing_metals,grep("Silver",data$plan_name))
		missing_silver73 <- intersect(missing_silver,grep("73",data$plan_name))
		missing_silver87 <- intersect(missing_silver,grep("87",data$plan_name))
		missing_silver94 <- intersect(missing_silver,grep("94",data$plan_name))
		missing_gold <- intersect(missing_metals,grep("Gold",data$plan_name))
		missing_platinum <- intersect(missing_metals,grep("Platinum",data$plan_name))
		missing_catastrophic <- intersect(missing_metals,grep("Minimum Coverage",data$plan_name))
		missing_other <- setdiff(missing_metals,c(missing_bronze,missing_silver,missing_gold,
			missing_platinum,missing_catastrophic)) # I checked by hand, this one record is silver 87
		
		data[missing_bronze,"metal_level_enhanced"] <- "Bronze"
		data[setdiff(missing_silver,c(missing_silver73,missing_silver87,missing_silver94)),
			"metal_level_enhanced"] <- "Silver"
		data[missing_silver73,"metal_level_enhanced"] <- "Silver - Enhanced 73"
		data[c(missing_other,missing_silver87),"metal_level_enhanced"] <- "Silver - Enhanced 87"
		data[missing_silver94,"metal_level_enhanced"] <- "Silver - Enhanced 94"
		data[missing_gold,"metal_level_enhanced"] <- "Gold"
		data[missing_platinum,"metal_level_enhanced"] <- "Platinum"
		data[missing_catastrophic,"metal_level_enhanced"] <- "Minimum Coverage"
		
		data$metal <- NA
		data[data$metal_level_enhanced == "Minimum Coverage","metal"] <- "Minimum Coverage"
		data[data$metal_level_enhanced == "Bronze","metal"] <- "Bronze"
		data[data$metal_level_enhanced %in%  c("Silver","Silver - Enhanced 73",
			"Silver - Enhanced 87","Silver - Enhanced 94"),"metal"] <- "Silver"
		data[data$metal_level_enhanced == "Gold","metal"] <- "Gold"
		data[data$metal_level_enhanced == "Platinum","metal"] <- "Platinum"
		
		# Age - flag missing and records with age over 120
		data[is.na(data$age) | data$age > 120,"flagged"] <- TRUE
				
		# Insurer
		data[data$insurer == "Anthem Blue Cross","insurer"] <- "Anthem"
		data[data$insurer == "Blue Shield","insurer"] <- "Blue_Shield"
		data[data$insurer == "Chinese Community","insurer"] <- "Chinese_Community"
		data[data$insurer == "Contra Costa Heal","insurer"] <- "Contra_Costa"
		data[data$insurer == "Health Net","insurer"] <- "Health_Net"
		data[data$insurer == "LA Care","insurer"] <- "LA_Care"
		data[data$insurer == "Molina Health Car","insurer"] <- "Molina"
		data[data$insurer == "Oscar Health Plan","insurer"] <- "Oscar"
		data[data$insurer == "SHARP Health Plan","insurer"] <- "SHARP"
		data[data$insurer == "UnitedHealthcare","insurer"] <- "United"
		data[data$insurer == "Valley Health","insurer"] <- "Valley"
		data[data$insurer == "Western Health","insurer"] <- "Western"	
		
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
		
		# 3 digit-zip
			# There are 451 records with missing 3 digit-zips
			# I'm just going to flag them
			
			data[is.na(data$zip3),"flagged"] <- TRUE
			
		# Create OEP vs. SEP Variable
			# 2014: OEP through week 17
			# 2015: OEP through week 9
			# 2016: OEP through week 9
			# 2017: OEP through week 9
			# 2018: OEP through week 8

		data$start_week <- as.numeric(substr(as.character(data$cov_start_dt_WK),6,7))
		data$end_week <- as.numeric(substr(as.character(data$cov_end_dt_WK),6,7))
		data$cov_start_dt_WK <- NULL
		data$cov_end_dt_WK <- NULL
		
		data$OEP <- TRUE
		data[data$year == 2014 & data$start_week > 17,"OEP"] <- FALSE
		data[data$year == 2015 & data$start_week > 9,"OEP"] <- FALSE
		data[data$year == 2016 & data$start_week > 9,"OEP"] <- FALSE
		data[data$year == 2017 & data$start_week > 9,"OEP"] <- FALSE
		data[data$year == 2018 & data$start_week > 8,"OEP"] <- FALSE
		data[data$year == 2019 & data$start_week > 4,"OEP"] <- FALSE
			
		
		# Race Group
		data$race <- NA
		data[data$race_ethnicity %in% c("American Indian","Native Hawaiian",
			"Other","Multiple Races"),"race"] <- "Other Race"
		data[data$race_ethnicity == "Latino","race"] <- "Hispanic"
		data[data$race_ethnicity == "Asian","race"] <- "Asian"
		data[data$race_ethnicity == "White","race"] <- "White"
		data[data$race_ethnicity == "Black or Africa","race"] <- "Black/African American"
			
			
	### Creation of IDs and Other Variables	(I need the plan ids to resolve region-county discrepancies)

		# Renumber individuals 1,...,I to save memory 
		# This code assigns each individual an integer
		individual_ids_names <- unique(data$individual_id)
		individual_ids <- 1:length(individual_ids_names)
		names(individual_ids) <- individual_ids_names
		data$individual_id <- individual_ids[as.character(data$individual_id)]
		rm(individual_ids,individual_ids_names)
			
		# Label records with individual id and year
			# Some records appear twice in a year
			# I'm going to use only their first choice
		
		data$id_year <- paste(data$individual_id,data$year,sep="_")
		midyear_transitions <- data[duplicated(data$id_year),"id_year"]
		data <- data[!data$id_year %in% midyear_transitions,]	
		rownames(data) <- paste(data$individual_id,data$year,sep="_")
		data$id_year <- NULL
		
		# Create plan identifier
			# For 2014 and 2015, I don't have HIOS2014, so I have to append the metal level
			# Plan IDs will be integers

		plan_ids <- 1:nrow(plan_data)
		names(plan_ids) <- paste(plan_data$HIOS,plan_data$metal_level,plan_data$ENROLLMENT_YEAR,plan_data$region,sep="_")
			
		data$hios_id_14 <- substr(data$hios_id_16,1,14)
		data$plan_id <- paste(data$hios_id_14,data$metal_level_enhanced,data$year,data$region,sep="_")
		data[data$year %in% c(2014,2015) & data$insurer != "SHARP","plan_id"] <- 
		paste(substr(data[data$year %in% c(2014,2015) & data$insurer != "SHARP","hios_id_14"],1,10),
			data[data$year %in% c(2014,2015) & data$insurer != "SHARP","metal_level_enhanced"],
			data[data$year %in% c(2014,2015) & data$insurer != "SHARP","year"],
			data[data$year %in% c(2014,2015) & data$insurer != "SHARP","region"],sep="_")
		data$plan_id <- plan_ids[data$plan_id]
		
			# We get some NAs here, let's do a little cleanup on the region variable
		
			# SHARP
			data[data$insurer == "SHARP","region"] <- 19
			
			# Valley
			data[data$insurer == "Valley","region"] <- 7
		
			# Chinese Community
			data[is.na(data$plan_id)  & data$insurer == "Chinese_Community" & data$zip3 == 940,"region"] <- 8
			data[is.na(data$plan_id)  & data$insurer == "Chinese_Community" & data$zip3 == 941,"region"] <- 4
			data[is.na(data$plan_id)  & data$insurer == "Chinese_Community" & data$zip3 == 944,"region"] <- 8
		
			# Western
			data[is.na(data$plan_id)  & data$insurer == "Western" & 
				data$hios_id_14 %in% c("93689CA0110001","93689CA0120001",
					"93689CA0110002","93689CA0120004","93689CA0130002","93689CA0120005"),"region"] <- 2
			data[is.na(data$plan_id)  & data$insurer == "Western" & 
				data$hios_id_14 %in% c("93689CA0150001","93689CA0160001",
					"93689CA0150002","93689CA0160002","93689CA0170001","93689CA0160003"),"region"] <- 3
			
			# Molina
			data[is.na(data$plan_id)  & data$insurer == "Molina" & data$zip3 == 922,"region"] <- 13
			
			# Oscar
			data[is.na(data$plan_id)  & data$insurer == "Oscar" & data$zip3 == 906,"region"] <- 18
			
			# Anthem
			data[is.na(data$plan_id)  & data$insurer == "Anthem" & data$zip3 == 959,"region"] <- 1
			data[is.na(data$plan_id)  & data$insurer == "Anthem" & data$zip3 == 960,"region"] <- 1
			data[is.na(data$plan_id)  & data$insurer == "Anthem" & data$zip3 == 932,"region"] <- 10
			data[is.na(data$plan_id)  & data$insurer == "Anthem" & data$zip3 == 940,"region"] <- 7
			data[is.na(data$plan_id)  & data$insurer == "Anthem" & data$zip3 == 950,"region"] <- 7
			data[is.na(data$plan_id)  & data$insurer == "Anthem" & data$zip3 == 951,"region"] <- 7
			data[is.na(data$plan_id)  & data$insurer == "Anthem" & data$zip3 == 952,"region"] <- 10
			data[is.na(data$plan_id)  & data$insurer == "Anthem" & data$zip3 == 953,"region"] <- 10
			data[is.na(data$plan_id)  & data$insurer == "Anthem" & data$zip3 == 954,"region"] <- 1
			data[is.na(data$plan_id)  & data$insurer == "Anthem" & data$zip3 == 955,"region"] <- 1
			
			# Kaiser - use observed distribution to assign a region
				
				# 954 - rating area 2
				data[is.na(data$plan_id)  & data$insurer == "Kaiser" & data$zip3 == 954,"region"] <- 2
				
				# 922 - rating area 17
				data[is.na(data$plan_id)  & data$insurer == "Kaiser" & data$zip3 == 922,"region"] <- 17
				
				# 935 - rating area 15
				data[is.na(data$plan_id)  & data$insurer == "Kaiser" & data$zip3 == 935,"region"] <- 15
				
				# 940 - could be rating areas 7,8
				set.seed(1)
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Kaiser" & 
					data$zip3 == 940,"region"])[as.character(c(7,8))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Kaiser" & data$zip3 == 940)
				data[fill_ids,"region"] <- sample(c(7,8),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 945 - could be rating areas 2,5,6
				set.seed(1)
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Kaiser" & 
					data$zip3 == 945,"region"])[as.character(c(2,5,6))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Kaiser" & data$zip3 == 945)
				data[fill_ids,"region"] <- sample(c(2,5,6),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 950 - could be rating areas 7 or 9
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Kaiser" & 
					data$zip3 == 950,"region"])[as.character(c(7,9))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Kaiser" & data$zip3 == 950)
				data[fill_ids,"region"] <- sample(c(7,9),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 906 - could be rating areas 15 or 18
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Kaiser" & 
					data$zip3 == 906,"region"])[as.character(c(15,18))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Kaiser" & data$zip3 == 906)
				data[fill_ids,"region"] <- sample(c(15,18),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
			# Health Net
		
				# 907 - rating area 15
				data[is.na(data$plan_id)  & data$insurer == "Health_Net" & data$zip3 == 907,"region"] <- 15
		
				# 913 - rating area 16
				data[is.na(data$plan_id)  & data$insurer == "Health_Net" & data$zip3 == 913,"region"] <- 16
		
				# 922 - rating area 17
				data[is.na(data$plan_id)  & data$insurer == "Health_Net" & data$zip3 == 922,"region"] <- 17
		
				# 935 - rating area 15
				data[is.na(data$plan_id)  & data$insurer == "Health_Net" & data$zip3 == 935,"region"] <- 15
		
		
				# 906 - could be rating areas 15 or 18
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Health_Net" & 
					data$zip3 == 906,"region"])[as.character(c(15,18))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Health_Net" & data$zip3 == 906)
				data[fill_ids,"region"] <- sample(c(15,18),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 917 - could be rating areas 15 or 17
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Health_Net" & 
					data$zip3 == 917,"region"])[as.character(c(15,17))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Health_Net" & data$zip3 == 917)
				data[fill_ids,"region"] <- sample(c(15,17),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 928 - could be rating areas 17 or 18
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Health_Net" & 
					data$zip3 == 928,"region"])[as.character(c(17,18))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Health_Net" & data$zip3 == 928)
				data[fill_ids,"region"] <- sample(c(17,18),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
			# Blue Shield
			
				# 906 - could be rating areas 15 or 18
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Blue_Shield" & 
					data$zip3 == 906,"region"])[as.character(c(15,18))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Blue_Shield" & data$zip3 == 906)
				data[fill_ids,"region"] <- sample(c(15,18),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 913 - could be rating areas 12 or 16
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Blue_Shield" & 
					data$zip3 == 913,"region"])[as.character(c(12,16))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Blue_Shield" & data$zip3 == 913)
				data[fill_ids,"region"] <- sample(c(12,16),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 917 - could be rating areas 15 or 17
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Blue_Shield" & 
					data$zip3 == 917,"region"])[as.character(c(15,17))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Blue_Shield" & data$zip3 == 917)
				data[fill_ids,"region"] <- sample(c(15,17),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 922 - could be rating areas 13 or 17
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Blue_Shield" & 
					data$zip3 == 922,"region"])[as.character(c(13,17))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Blue_Shield" & data$zip3 == 922)
				data[fill_ids,"region"] <- sample(c(13,17),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 928 - could be rating areas 17 or 18
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Blue_Shield" & 
					data$zip3 == 928,"region"])[as.character(c(17,18))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Blue_Shield" & data$zip3 == 928)
				data[fill_ids,"region"] <- sample(c(17,18),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 932 - could be rating areas 10,11,14
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Blue_Shield" & 
					data$zip3 == 932,"region"])[as.character(c(10,11,14))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Blue_Shield" & data$zip3 == 932)
				data[fill_ids,"region"] <- sample(c(10,11,14),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 935 - could be rating areas 13,14,15,17
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Blue_Shield" & 
					data$zip3 == 935,"region"])[as.character(c(13,14,15,17))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Blue_Shield" & data$zip3 == 935)
				data[fill_ids,"region"] <- sample(c(13,14,15,17),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 940 - could be rating areas 7,8
				set.seed(1)
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Blue_Shield" & 
					data$zip3 == 940,"region"])[as.character(c(7,8))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Blue_Shield" & data$zip3 == 940)
				data[fill_ids,"region"] <- sample(c(7,8),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 945 - could be rating areas 2,5,6
				set.seed(1)
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Blue_Shield" & 
					data$zip3 == 945,"region"])[as.character(c(2,5,6))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Blue_Shield" & data$zip3 == 945)
				data[fill_ids,"region"] <- sample(c(2,5,6),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 950 - could be rating areas 7 or 9
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Blue_Shield" & 
					data$zip3 == 950,"region"])[as.character(c(7,9))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Blue_Shield" & data$zip3 == 950)
				data[fill_ids,"region"] <- sample(c(7,9),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 954 - could be rating areas 1 or 2
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Blue_Shield" & 
					data$zip3 == 954,"region"])[as.character(c(1,2))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Blue_Shield" & data$zip3 == 954)
				data[fill_ids,"region"] <- sample(c(1,2),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
				
				# 961 - could be rating areas 1 or 3
				obs_dist <- table(data[!is.na(data$plan_id) & data$insurer == "Blue_Shield" & 
					data$zip3 == 961,"region"])[as.character(c(1,3))]
				obs_dist <- obs_dist/sum(obs_dist)
				
				fill_ids <- which(is.na(data$plan_id) & data$insurer == "Blue_Shield" & data$zip3 == 961)
				data[fill_ids,"region"] <- sample(c(1,3),length(fill_ids),prob=obs_dist,replace=TRUE)
				data[fill_ids,"flagged"] <- TRUE 
		
			data$plan_id <- paste(data$hios_id_14,data$metal_level_enhanced,data$year,data$region,sep="_")
			data[data$year %in% c(2014,2015) & data$insurer != "SHARP","plan_id"] <- 
			paste(substr(data[data$year %in% c(2014,2015) & data$insurer != "SHARP","hios_id_14"],1,10),
				data[data$year %in% c(2014,2015) & data$insurer != "SHARP","metal_level_enhanced"],
				data[data$year %in% c(2014,2015) & data$insurer != "SHARP","year"],
				data[data$year %in% c(2014,2015) & data$insurer != "SHARP","region"],sep="_")
			data$plan_id <- plan_ids[data$plan_id]
			
		# There are about 17,000 records without a plan - flag them
		data[is.na(data$plan_id),"flagged"] <- TRUE
		
	
		data$premium21 <- plan_data[data$plan_id,"Premium"]/1.278
		data$plan_name <- plan_data[data$plan_id,"Plan_Name2"]
				

		# For Health Net, check HMO vs. HSP

		hsp_regions <- c(1,3,7,11)
		partial_hsp_regions <- c(14:19)
		
		data$plan_network_type <- as.character(data$plan_network_type)
		data[data$region %in% hsp_regions & data$insurer == "Health_Net" & 
			data$year %in% c(2016,2017),"plan_network_type"] <- "HSP"
		data[data$region %in% partial_hsp_regions & data$insurer == "Health_Net" & 
			data$year %in% c(2016,2017,2018,2019) & data$plan_network_type == "HMO" & 
			data$metal %in% c("Minimum Coverage","Bronze"),
			"plan_network_type"] <- "HSP"

# Geographic Checks	
			
	# Check that all region/3-digit zip/year combinations are valid
	data$zip_region <- paste(data$zip3,data$region,sep="_")
	data$zip_region_year <- paste(data$zip3,data$region,data$year,sep="_")
	
		# 51,980 records (0.5%) do not have a valid zip/region combination
		# I'm going to flag these records rather than try to reassign a 3-digit zip
		# They are fine to use for summary stats, but should not be used for estimation/simulations
		
		data[!data$zip_region %in% paste(zip3_choices$zip3,zip3_choices$Region,sep="_"),
			"flagged"] <- TRUE
	
	# Check that the chosen plan is in the consumer's geographic choice set
		# Amazingly, all but 11,976 records (0.1%) are on a plan in their choice set
		# I'm going to flag these records
		
		product_definitions$insurer <- as.character(product_definitions$insurer)
		product_definitions$plan_network_type <- as.character(product_definitions$plan_network_type)
		single_product_insurers <- c("Chinese_Community","Contra_Costa","Kaiser","LA_Care","Molina",
				"Oscar","United","Valley","Western")
	
		data$plan_check <- NA
		zipchoices <- as.matrix(zip3_choices[,4:ncol(zip3_choices)])
		
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
			
			data[data$zip_region_year == i,"plan_check"] <- 
				data[data$zip_region_year == i,"plan_id"] %in% available_plans	
		}
		
		data[!data$plan_check & !is.na(data$plan_check),"flagged"] <- TRUE
		data$plan_check <- NULL
			
# Income Checks
	
	# subsidy_fpl_bracket
			# Use ACS plans to fill in missing values
			# Fix some inconsistencies (e.g., on an ACS plan, but income > 400% of FPL)
			# This still leaves about 157,000 missing values (fill in when reconciling with the net premium)
		
		data$subsidy_eligible <- as.character(data$subsidy_eligible)
		data$subsidy_fpl_bracket <- as.character(data$subsidy_fpl_bracket)		
		data$old_subsidy_fpl_bracket <- data$subsidy_fpl_bracket
		
		# Use the FPL variable to fill in bracket
		data[data$subsidy_fpl_percent_int > 0 & data$subsidy_fpl_percent_int <= 138 & 
			!is.na(data$subsidy_fpl_percent_int) & data$subsidy_fpl_bracket %in%
			c("FPL Unavailable","Unsubsidized Applica"),"subsidy_fpl_bracket"] <- 
			"138% FPL or less"
		data[data$subsidy_fpl_percent_int > 138 & data$subsidy_fpl_percent_int <= 150 & 
			!is.na(data$subsidy_fpl_percent_int) & data$subsidy_fpl_bracket %in%
			c("FPL Unavailable","Unsubsidized Applica"),"subsidy_fpl_bracket"] <- 
			"138% FPL to 150% FPL"
		data[data$subsidy_fpl_percent_int > 150 & data$subsidy_fpl_percent_int <= 200 & 
			!is.na(data$subsidy_fpl_percent_int) & data$subsidy_fpl_bracket %in%
			c("FPL Unavailable","Unsubsidized Applica"),"subsidy_fpl_bracket"] <- 
			"150% FPL to 200% FPL"
		data[data$subsidy_fpl_percent_int > 200 & data$subsidy_fpl_percent_int <= 250 & 
			!is.na(data$subsidy_fpl_percent_int) & data$subsidy_fpl_bracket %in%
			c("FPL Unavailable","Unsubsidized Applica"),"subsidy_fpl_bracket"] <- 
			"200% FPL to 250% FPL"
		data[data$subsidy_fpl_percent_int > 250 & data$subsidy_fpl_percent_int <= 400 & 
			!is.na(data$subsidy_fpl_percent_int) & data$subsidy_fpl_bracket %in%
			c("FPL Unavailable","Unsubsidized Applica"),"subsidy_fpl_bracket"] <- 
			"250% FPL to 400% FPL"
		data[data$subsidy_fpl_percent_int > 400 & !is.na(data$subsidy_fpl_percent) & 
			data$subsidy_fpl_bracket %in% c("FPL Unavailable","Unsubsidized Applica"),
			"subsidy_fpl_bracket"] <- 
			"400% FPL or greater"
			
		# Silver Enhanced plans
			# At this point, only those with missing FPL values do not have a bracket assigned
			# 1) For any FPL unavailable or unsubsidized application records with an acs plan, assign
				# them to the appropriate bracket
			# 2) If any records have an FPL that is inconsistent with their ACS plan, reassign them	
				# to the appropriate plan (this only involves 29,000 records - 0.4%)
		
		acs_plans <- c("Silver - Enhanced 73","Silver - Enhanced 87","Silver - Enhanced 94")
		
		assign_silver73 <- which(data$metal_level_enhanced == "Silver - Enhanced 73" &
			data$subsidy_fpl_bracket %in% c("FPL Unavailable","Unsubsidized Applica"))
		assign_silver87 <- which(data$metal_level_enhanced == "Silver - Enhanced 87" &
			data$subsidy_fpl_bracket %in% c("FPL Unavailable","Unsubsidized Applica"))
		assign_silver94 <- which(data$metal_level_enhanced == "Silver - Enhanced 94" &
			data$subsidy_fpl_bracket %in% c("FPL Unavailable","Unsubsidized Applica"))
			
		data[assign_silver73,"subsidy_fpl_bracket"] <- "200% FPL to 250% FPL"
		data[assign_silver87,"subsidy_fpl_bracket"] <- "150% FPL to 200% FPL"
		data[assign_silver94,"subsidy_fpl_bracket"] <- "150% FPL or less"
		
		wrong_silver73 <- which(data$metal_level_enhanced == "Silver - Enhanced 73" &
			data$subsidy_fpl_bracket != "200% FPL to 250% FPL")
		wrong_silver87 <- which(data$metal_level_enhanced == "Silver - Enhanced 87" &
			data$subsidy_fpl_bracket != "150% FPL to 200% FPL")
		wrong_silver94 <- which(data$metal_level_enhanced == "Silver - Enhanced 94" &
			!data$subsidy_fpl_bracket %in% c("138% FPL or less","138% FPL to 150% FPL","150% FPL or less"))
		
		data[intersect(which(data$subsidy_fpl_bracket == "200% FPL to 250% FPL"),
			c(wrong_silver87,wrong_silver94)),"metal_level_enhanced"] <- "Silver - Enhanced 73"		
		data[intersect(which(data$subsidy_fpl_bracket == "150% FPL to 200% FPL"),
			c(wrong_silver73,wrong_silver94)),"metal_level_enhanced"] <- "Silver - Enhanced 87"	
		data[intersect(which(data$subsidy_fpl_bracket %in% c("138% FPL or less","138% FPL to 150% FPL","150% FPL or less")),
			c(wrong_silver73,wrong_silver87)),"metal_level_enhanced"] <- "Silver - Enhanced 94"		
		data[intersect(which(!data$subsidy_fpl_bracket %in% 
			c("138% FPL or less","138% FPL to 150% FPL","150% FPL or less","150% FPL to 200% FPL","200% FPL to 250% FPL")),
			c(wrong_silver73,wrong_silver87,wrong_silver94)),"metal_level_enhanced"] <- "Silver"		
		
		# Assign people to > 400% who meet the following criteria:
				# subsidy_fpl_bracket FPL Unavailable or Unsubsidized Applica
				# unsubsidized 
				# gross premium amount = net premium amount 
			
			assign_to_gt400 <- which(data$subsidy_eligible != "Subsidy Eligible" & 
				data$subsidy_fpl_bracket %in% c("Unsubsidized Applica","FPL Unavailable") & 
				!is.na(data$net_premium_amt_int) & 
				data$gross_premium_amt_int == data$net_premium_amt_int)
			
			data[assign_to_gt400,"subsidy_fpl_bracket"] <- "400% FPL or greater"
			data[assign_to_gt400,"subsidy_eligible"] <- "Not Subsidy Elig"
			data[assign_to_gt400,"aptc_amt_int"] <- 0
		
		# Assign people to > 400% who meet the following critera:
				# subsidy_fpl_braket Unsubsidized applica
				# gross premium amount = net premium amount 
				
			assign_to_gt400 <- which(data$subsidy_fpl_bracket %in% c("Unsubsidized Applica") & 
				!is.na(data$net_premium_amt_int) & 
				data$gross_premium_amt_int == data$net_premium_amt_int)
				
			data[assign_to_gt400,"subsidy_fpl_bracket"] <- "400% FPL or greater"
			data[assign_to_gt400,"subsidy_eligible"] <- "Not Subsidy Elig"
			data[assign_to_gt400,"aptc_amt_int"] <- 0
		
		# There are 479 "Unsubsidized Applications" for which GROSS > NET
			# This is wrong, so classify as "400% FPL or less"
			
			data[data$subsidy_fpl_bracket == "Unsubsidized Applica","subsidy_eligible"] <- "Subsidy Eligible"
			data[data$subsidy_fpl_bracket == "Unsubsidized Applica","subsidy_fpl_bracket"] <- "400% FPL or less"
		
		# If GROSS > NET, then classify as < 400% of FPL
			data[data$gross_premium_amt_int > data$net_premium_amt_int & !is.na(data$gross_premium_amt_int) & 
				data$subsidy_fpl_bracket == "FPL Unavailable","subsidy_eligible"] <- "Subsidy Eligible"
			data[data$gross_premium_amt_int > data$net_premium_amt_int & !is.na(data$gross_premium_amt_int) & 
				data$subsidy_fpl_bracket == "FPL Unavailable","subsidy_fpl_bracket"] <- "400% FPL or less"
		
		# Anyone remaining who is classified as "subsidized", assign to < 400% of FPL
			data[data$subsidy_eligible == "Subsidy Eligible" & !is.na(data$subsidy_eligible) & 
				data$subsidy_fpl_bracket == "FPL Unavailable","subsidy_fpl_bracket"] <- "400% FPL or less"
		
		# There are 0 records left!  All we know about them is that they didn't get a subsidy	
		# I'm going to assign them to > 400% of FPL
		
			#data[data$subsidy_fpl_bracket == "FPL Unavailable","aptc_amt_int"] <- 0
			#data[data$subsidy_fpl_bracket == "FPL Unavailable","subsidy_eligible"] <- "UNSUBSIDIZ"
			#data[data$subsidy_fpl_bracket == "FPL Unavailable","subsidy_fpl_bracket"] <- "400% FPL or greater"
		
		# There are no records that still don't have a subsidy status!
			
			#data[is.na(data$subsidy_eligible) & data$aptc_amt_int > 0 & !is.na(data$aptc_amt_int),
			#	"subsidy_eligible"] <- "SUBSIDIZED"
			#data[is.na(data$subsidy_eligible) & data$aptc_amt_int == 0 & !is.na(data$aptc_amt_int),
			#	"subsidy_eligible"] <- "UNSUBSIDIZ"
			#data[is.na(data$subsidy_eligible) & data$gross_premium_amt_int == data$net_premium_amt_int &
			#	!is.na(data$net_premium_amt_int),"subsidy_eligible"] <- "UNSUBSIDIZ"
				
		# Subsidized/Unsubsidized - No one over 400% of FPL should be subsidized
		data[data$subsidy_eligible == "Subsidy Eligible" & !is.na(data$subsidy_eligible) & 
				data$subsidy_fpl_bracket == "400% FPL or greater" & 
				data$gross_premium_amt_int > data$net_premium_amt_int & !is.na(data$net_premium_amt_int),"subsidy_fpl_bracket"] <- "400% FPL or less"
		data[data$subsidy_eligible == "Subsidy Eligible" & !is.na(data$subsidy_eligible) & 
				data$subsidy_fpl_bracket == "400% FPL or greater" & 
				data$gross_premium_amt_int == data$net_premium_amt_int & !is.na(data$net_premium_amt_int),"subsidy_eligible"] <- "Not Subsidy Elig"

				
		# I also need to worry about people with zero income	
		
		data[data$subsidy_fpl_percent_int == 0 & !is.na(data$subsidy_fpl_percent_int) &
			!data$subsidy_fpl_bracket %in% c("138% FPL or less","150% FPL or less"),
			"subsidy_fpl_percent_int"] <- NA

			
		gc()
			
	
		
### Creation of Households
	# Some cases could contain multiple households
	# Potential red flags:
		# Lots of adults over age 25 (start with 4 or more)
		# Large household size (start with 6 or more)
		# Multiple plans chosen by a case (2 or more)
				
	# Use this flag to keep track of records that are good to go
	data$placed_in_household <- FALSE	
		
	# Fill in missing case ids - none are missing in this data extract (lots were in previous)
	
	data$ahbx_case_id_x <- as.character(data$ahbx_case_id_x)
	
	# Now we compute summary statistics for each case
	
		data$case_year <- paste(data$ahbx_case_id_x,data$year,sep="")
	
		compute.number.unique <- function(x) {
			return(length(unique(x)))
		}
		
		# Members per case id
		members_per_case <- by(data$individual_id,data$case_year,length)
		gc()
		
		# Income categories per case id
		incomes_per_case <- by(data$aptc_amt_int,data$case_year,compute.number.unique)
		gc()
		
		# Plans purchased for each case id	
		plans_per_case <- by(data$plan_name,data$case_year,compute.number.unique)
		gc()
		
		# Number of Different Gross/Net Premiums per case
		number_gross_per_case <- by(data$gross_premium_amt_int,data$case_year,compute.number.unique) 
		number_net_per_case <- by(data$net_premium_amt_int,data$case_year,compute.number.unique) 
		gc()
		
		# Regions per case
		regions_per_case <- by(data$region,data$case_year,compute.number.unique) 
		zips_per_case <- by(data$zip3,data$case_year,compute.number.unique) 
		gc()
		
		# Problem households - There are about 132,000 households or 1.4% affected
		problem_households <- unique(c(names(incomes_per_case)[incomes_per_case > 1],
								names(zips_per_case)[zips_per_case > 1],
								names(regions_per_case)[regions_per_case > 1],
								names(number_net_per_case)[number_net_per_case > 1],
								names(number_gross_per_case)[number_gross_per_case > 1],
								names(plans_per_case)[plans_per_case > 1]))
		
	# Can we use households to fill in missing zip values?
		# In this extract, there are only 451 records and they are all flagged
		
		#determine_fill <- function(x,data) {
		#	zips <- data[data$case_year == x & !is.na(data$zip3),"zip3"]
		#	if(compute.number.unique(zips) == 1 & length(zips) > 0) {
		#		return(zips[1])
		#	} else {
		#		return(NA)
		#	}
		#}
		
		#problem_pop <- data[data$case_year %in% problem_households,c("case_year","zip3")]
		
		#fill_ins <- intersect(problem_households,
		#				intersect(names(zips_per_case)[zips_per_case == 2],
		#				names(regions_per_case)[regions_per_case == 1]))
		#fill_in_status <- sapply(fill_ins,FUN=determine_fill,problem_pop)
		#fill_in_status <- fill_in_status[!is.na(fill_in_status)]
		
		#data[data$case_year %in% names(fill_in_status),"zip3"] <- 
		#	fill_in_status[data[data$case_year %in% names(fill_in_status),"case_year"]]
		
		#zips_per_case[names(fill_in_status)] <- 
		#	by(data[data$case_year %in% names(fill_in_status),"zip3"],
		#		data[data$case_year %in% names(fill_in_status),"case_year"],compute.number.unique) 
		
		#problem_households <- unique(c(names(incomes_per_case)[incomes_per_case > 1],
		#						names(zips_per_case)[zips_per_case > 1],
		#						names(regions_per_case)[regions_per_case > 1],
		#						names(number_net_per_case)[number_net_per_case > 1],
		#						names(number_gross_per_case)[number_gross_per_case > 1],
		#						names(plans_per_case)[plans_per_case > 1]))
		
		
		
	# Can we use households to fill in missing income values?
		# Missing values - 400% FPL or less, 150% FPL or less 
	
		data$aptc_amt_int <- pmax(0,data$gross_premium_amt_int - data$net_premium_amt_int)
	
		determine_fill_income <- function(x,data) {
			incomes <- data[data$case_year == x & !is.na(data$aptc_amt_int),"aptc_amt_int"]	
			if(compute.number.unique(incomes) == 1 & length(incomes) > 0) {
				return(incomes[1])
			} else {
				return(NA)
			}
		}
	
		problem_pop <- data[data$case_year %in% problem_households,c("case_year","aptc_amt_int")]
	
		fill_ins <- Reduce(intersect,list(names(incomes_per_case)[incomes_per_case == 2],
								names(zips_per_case)[zips_per_case == 1],
								names(regions_per_case)[regions_per_case == 1],
								names(number_net_per_case)[number_net_per_case == 1],
								names(number_gross_per_case)[number_gross_per_case == 1],
								names(plans_per_case)[plans_per_case == 1]))
		fill_in_status <- sapply(fill_ins,FUN=determine_fill_income,problem_pop)
		fill_in_status <- fill_in_status[!is.na(fill_in_status)]
		
		data[data$case_year %in% names(fill_in_status),"aptc_amt_int"] <- 
			fill_in_status[data[data$case_year %in% names(fill_in_status),"case_year"]]
		
		incomes_per_case[names(fill_in_status)] <- 
			by(data[data$case_year %in% names(fill_in_status),"aptc_amt_int"],
				data[data$case_year %in% names(fill_in_status),"case_year"],compute.number.unique)
	
		# Use the FPL variable to fill in bracket
		data[data$subsidy_fpl_percent_int > 0 & data$subsidy_fpl_percent_int <= 138 & 
			!is.na(data$subsidy_fpl_percent_int) & data$subsidy_fpl_bracket %in%
			c("FPL Unavailable","Unsubsidized Applica"),"subsidy_fpl_bracket"] <- 
			"138% FPL or less"
		data[data$subsidy_fpl_percent_int > 138 & data$subsidy_fpl_percent_int <= 150 & 
			!is.na(data$subsidy_fpl_percent_int) & data$subsidy_fpl_bracket %in%
			c("FPL Unavailable","Unsubsidized Applica"),"subsidy_fpl_bracket"] <- 
			"138% FPL to 150% FPL"
		data[data$subsidy_fpl_percent_int > 150 & data$subsidy_fpl_percent_int <= 200 & 
			!is.na(data$subsidy_fpl_percent_int) & data$subsidy_fpl_bracket %in%
			c("FPL Unavailable","Unsubsidized Applica"),"subsidy_fpl_bracket"] <- 
			"150% FPL to 200% FPL"
		data[data$subsidy_fpl_percent_int > 200 & data$subsidy_fpl_percent_int <= 250 & 
			!is.na(data$subsidy_fpl_percent_int) & data$subsidy_fpl_bracket %in%
			c("FPL Unavailable","Unsubsidized Applica"),"subsidy_fpl_bracket"] <- 
			"200% FPL to 250% FPL"
		data[data$subsidy_fpl_percent_int > 250 & data$subsidy_fpl_percent_int <= 400 & 
			!is.na(data$subsidy_fpl_percent_int) & data$subsidy_fpl_bracket %in%
			c("FPL Unavailable","Unsubsidized Applica"),"subsidy_fpl_bracket"] <- 
			"250% FPL to 400% FPL"
		data[data$subsidy_fpl_percent_int > 400 & !is.na(data$subsidy_fpl_percent) & 
			data$subsidy_fpl_bracket %in% c("FPL Unavailable","Unsubsidized Applica"),
			"subsidy_fpl_bracket"] <- 
			"400% FPL or greater"
	
	# Now I am going to start splitting households
	
		data$split_flag <- FALSE
	
		# Create household-year variable
			# For ease of coding, I'm going to renumber the households
			
			# Renumber households 1,...,H to save memory 
			# This code assigns each household an integer
			household_ids_names <- unique(data$ahbx_case_id_x)
			household_ids <- 1:length(household_ids_names)
			names(household_ids) <- household_ids_names
			data$household_id <- household_ids[as.character(data$ahbx_case_id_x)]
			rm(household_ids,household_ids_names)
			data$household_year <- paste(data$household_id,data$year,sep="_")
			household_counter <- max(data$household_id) 
	
		# 1) Household has multiple premiums and multiple plans
		
		split_candidates <- Reduce(intersect,list(names(number_gross_per_case)[number_gross_per_case > 1],
								names(plans_per_case)[plans_per_case > 1]))
		split_individuals <- which(data$case_year %in% split_candidates) 
	
		data$household_year_premium_plan <- paste(data$household_year,data$gross_premium_amt_int,data$plan_name,sep="")
		new_households <- unique(data[split_individuals,"household_year_premium_plan"])
		new_ids <- (household_counter+1):(household_counter + length(new_households))
		names(new_ids) <- new_households
		
		data[split_individuals,"household_id"] <- new_ids[data[split_individuals,"household_year_premium_plan"]]
		data[split_individuals,"household_year"] <- paste(data[split_individuals,"household_id"],
			data[split_individuals,"year"],sep="_")
		data$household_year_premium_plan <- NULL
		data[split_individuals,"split_flag"] <- TRUE
		household_counter <- max(data$household_id) 
		
		incomes_per_case <- by(data$aptc_amt_int,data$household_year,compute.number.unique)
		plans_per_case <- by(data$plan_name,data$household_year,compute.number.unique)
		number_gross_per_case <- by(data$gross_premium_amt_int,data$household_year,compute.number.unique) 
		number_net_per_case <- by(data$net_premium_amt_int,data$household_year,compute.number.unique) 
		regions_per_case <- by(data$region,data$household_year,compute.number.unique) 
		zips_per_case <- by(data$zip3,data$household_year,compute.number.unique) 

		problem_households <- unique(c(names(incomes_per_case)[incomes_per_case > 1],
								names(zips_per_case)[zips_per_case > 1],
								names(regions_per_case)[regions_per_case > 1],
								names(number_net_per_case)[number_net_per_case > 1],
								names(number_gross_per_case)[number_gross_per_case > 1],
								names(plans_per_case)[plans_per_case > 1]))	
								
			# There are about 65,000 households remaining 
	
		# 2) Household has multiple income brackets and multiple premiums
	
		split_candidates <- Reduce(intersect,list(names(incomes_per_case)[incomes_per_case >= 2],
								names(number_gross_per_case)[number_gross_per_case > 1]))
		split_individuals <- which(data$household_year %in% split_candidates) 
	
		data$household_year_premium_fpl <- paste(data$household_year,data$gross_premium_amt_int,data$aptc_amt_int,sep="")
		new_households <- unique(data[split_individuals,"household_year_premium_fpl"])
		new_ids <- (household_counter+1):(household_counter + length(new_households))
		names(new_ids) <- new_households
		
		data[split_individuals,"household_id"] <- new_ids[data[split_individuals,"household_year_premium_fpl"]]
		data[split_individuals,"household_year"] <- paste(data[split_individuals,"household_id"],
			data[split_individuals,"year"],sep="_")
		data$household_year_premium_fpl <- NULL
		data[split_individuals,"split_flag"] <- TRUE
		household_counter <- max(data$household_id) 
		
		incomes_per_case <- by(data$aptc_amt_int,data$household_year,compute.number.unique)
		plans_per_case <- by(data$plan_name,data$household_year,compute.number.unique)
		number_gross_per_case <- by(data$gross_premium_amt_int,data$household_year,compute.number.unique) 
		number_net_per_case <- by(data$net_premium_amt_int,data$household_year,compute.number.unique) 
		regions_per_case <- by(data$region,data$household_year,compute.number.unique) 
		zips_per_case <- by(data$zip3,data$household_year,compute.number.unique) 

		problem_households <- unique(c(names(incomes_per_case)[incomes_per_case > 1],
								names(zips_per_case)[zips_per_case > 1],
								names(regions_per_case)[regions_per_case > 1],
								names(number_net_per_case)[number_net_per_case > 1],
								names(number_gross_per_case)[number_gross_per_case > 1],
								names(plans_per_case)[plans_per_case > 1]))	
									
	
		# 3) About 0.3% of the records remain in the problem households
	
			# I'm going to flag all of these people (except those with multiple plans per case and no other issues
			
			multiple_plans_ok <- Reduce(intersect,list(names(incomes_per_case)[incomes_per_case == 1],
								names(zips_per_case)[zips_per_case == 1],
								names(regions_per_case)[regions_per_case == 1],
								names(number_net_per_case)[number_net_per_case == 1],
								names(number_gross_per_case)[number_gross_per_case == 1],
								names(plans_per_case)[plans_per_case > 1]))
			problem_households <- setdiff(problem_households,multiple_plans_ok)
			
			data[data$household_year %in% problem_households,"flagged"] <- TRUE
		
	# Make sure no one > 400% of FPL gets subsidies
	data[data$subsidy_fpl_bracket == "400% FPL or greater","subsidy_eligible"] <- "Not Subsidy Elig"
	
	# Create members variable
	members_per_case <- by(data$household_year,data$household_year,length)
	orig_members_per_case <- by(data$case_year,data$case_year,length)
	
	data$members <- members_per_case[data$household_year]
	data$orig_members <- orig_members_per_case[data$case_year]
	
	# Let's save the data now
	save(data,file="data/final/enroll_temp")	
	gc()
		
		
		
##### Singles (Income-Net Premium Reconciliation)
	
	# Reload data
	data <- get(load("data/final/enroll_temp"))
	plan_data <- read.csv("data/plan_data.csv") # Covered California plan data
	age_rating_factors <- read.csv("data/age_rating_factors.csv",row.names=1) # CCIIO default rating curve
	poverty_guidelines <- read.csv("data/poverty_guidelines.csv",row.names=1) # Poverty guidelines
	contribution_percentages <- read.csv("data/contribution_percentages.csv",row.names = 1) # ACA contribution percentages
	rating_areas <- read.csv("data/rating_areas.csv",row.names = 1) # California county-rating area mapping
	zip3_choices <- read.csv("data/zip3_choices.csv",row.names = 1) # choice set by 3 digit zip and rating area
	product_definitions <- read.csv("data/product_definitions.csv",row.names = 1) # definitions of column names in zip3_choices
	
	set.seed(6)
	
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
	
	singles <- data[data$members == 1,]
	
	# Some records have a negative subsidized premium
	singles$net_premium_amt_int <- pmax(0,singles$net_premium_amt_int) 
	
	# Compute subsidy
		# Very close to the variable aptc_amt_int
		# has zeros instead of NAs
		
	singles$subsidy <- pmax(0,singles$gross_premium_amt_int - singles$net_premium_amt_int)
	
	# Compute second-lowest silver premium 
	
		get_available_silver_plans <- function(i,zip3_choices) {
		
			region_year_plans <- which(plan_data$ENROLLMENT_YEAR == zip3_choices[i,"Year"] &
				plan_data$region == zip3_choices[i,"Region"] & plan_data$metal_level == "Silver")
		
			# Get the broad product categories available (insurer/network type/etc. - no metal)
			available_products <- zipchoices[i,]
			available_products <- names(available_products)[!is.na(available_products)]
			available_insurers <- as.character(unique(product_definitions[available_products,"insurer"]))
			
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
			return(available_plans)
		}
		
		compute_second_lowest <- function(i,zip3_choices) {
		
			# Get available silver plans
			available_plans <- get_available_silver_plans(i,zip3_choices)
			silver_premiums <- sort(plan_data[available_plans,"Premium"])/1.278
				
			if(length(silver_premiums) == 1) {
				return(silver_premiums[1])
			} else {
				return(silver_premiums[2])
			}
		}	
	
		single_product_insurers <- c("Chinese_Community","Contra_Costa","Kaiser","LA_Care","Molina",
				"Oscar","United","Valley","Western")
		zipchoices <- as.matrix(zip3_choices[,4:ncol(zip3_choices)])
		zip3_choices$premiumSLC <- sapply(rownames(zip3_choices),FUN=compute_second_lowest,zip3_choices)
	
		singles$rating_factor <- 
			age_rating_factors[as.character(pmin(64,singles$age)),"Rating_Factor"]
		singles[singles$year >= 2018,"rating_factor"] <- 
			age_rating_factors[as.character(pmin(64,singles[singles$year >= 2018,"age"])),"Rating_Factor2018"]
		singles$premiumSLC <- zip3_choices[singles$zip_region_year,"premiumSLC"] * singles$rating_factor
		
	
	# Compute required contribution towards SLC silver plan
	singles$SLC_contribution <- pmax(0,singles$premiumSLC - singles$subsidy) 
	
	# There are about 3,500 records for which the individual receives a positive subsidy, but the FPL bracket is > 400%
		# For those whose original subsidy bracket was > 400% of FPL, set NET premium = GROSS PREMIUM and subsidy to 0
		# Flag those whose original subsidy bracket was FPL unavailable or Unsubsidized application
		
	singles[singles$subsidy_fpl_bracket == "400% FPL or greater" & singles$old_subsidy_fpl_bracket == "400% FPL or greater" & 
		singles$subsidy > 0 & !is.na(singles$subsidy),"net_premium_amt_int"] <- 
	singles[singles$subsidy_fpl_bracket == "400% FPL or greater" & singles$old_subsidy_fpl_bracket == "400% FPL or greater" & 
		singles$subsidy > 0 & !is.na(singles$subsidy),"gross_premium_amt_int"]
	singles$subsidy <- singles$gross_premium_amt_int - singles$net_premium_amt_int
		
	singles[singles$subsidy_fpl_bracket == "400% FPL or greater" & singles$old_subsidy_fpl_bracket != "400% FPL or greater" & 
		singles$subsidy > 0 & !is.na(singles$subsidy),"flagged"] <- TRUE
		
	# There is a major problem here:
		# Some people have effectively zero premium (I believe each person has to pay at least $1)
		# People use their subsidies to buy a bronze plan (or the lowest-cost silver plan)
		# In many cases, the subsidy based on SLC is bigger than the premium, so it ends up getting reduced
		# So the smaller subsidy leads to a larger SLC contribution, which leads to higher estimated income, which
		# will inevitably lead to higher estimated premiums faced by the household.
		
		# Solution: let's reconcile below without imputing household size for these people
		# This income level is an upper bound on income.  It may not even be a tight upper bound
		# if it is above the household's income range 	
		
	threshold_for_zero <- 3
	singles$zero_premium <- FALSE
	singles[singles$net_premium_amt_int <= threshold_for_zero & 
		!is.na(singles$net_premium_amt_int),"zero_premium"] <- TRUE 
		
	# Determine income piece to search
		# There are a lot of errors when reconciling the subsidy_fpl_bracket variable with the implied subsidy bracket (based on subsidy received)
		# In about 80 percent of the errors, the implied subsidy bracket is above the subsidy_fpl_bracket variable
		# This is because we assume that single enrollees are single households
		# But an individual member of a multi-person household could enroll in the exchange while the other family members do not
		# The contribution for a member of a multi-person household is higher than a single-household enrollees
		# Below, we will reconcile the implied subsidy and subsidy_fpl_bracket variable by imputing household size
	
		income_pieces <- c("138% FPL or less","138% FPL to 150% FPL","150% FPL to 200% FPL",
			"200% FPL to 250% FPL","250% FPL to 300% FPL","300% FPL to 400% FPL","400% FPL or greater")
		income_breakpoints <- c(0,1.38,1.5,2,2.5,3,4)
		max_imputation_size <- 5
		
		singles$subsidy_linear_piece <- NA
		singles$implied_subsidy_bracket <- NA
		singles$implied_household_size <- 1
		
		# Income check dummy (NA for those receiving no subsidy or those for whom the subsidy is NA)
		singles$income.check <- FALSE
		singles[singles$subsidy == 0 | is.na(singles$subsidy),"income.check"] <- NA
		singles[!is.na(singles$subsidy_fpl_percent_int),"income.check"] <- TRUE
		
		for(i in 1:max_imputation_size) {
		
			contribution_bounds <- cbind(contribution_percentages[,"YR2014"] * poverty_guidelines[i,"YR2014"],
											contribution_percentages[,"YR2015"] * poverty_guidelines[i,"YR2015"],
											contribution_percentages[,"YR2016"] * poverty_guidelines[i,"YR2016"],
											contribution_percentages[,"YR2017"] * poverty_guidelines[i,"YR2017"],
											contribution_percentages[,"YR2018"] * poverty_guidelines[i,"YR2018"],
											contribution_percentages[,"YR2019"] * poverty_guidelines[i,"YR2019"])/12 * income_breakpoints
			colnames(contribution_bounds) <- c("YR2014","YR2015","YR2016","YR2017","YR2018","YR2019")
			
			# Get linear piece
			singles[singles$year == 2014 & !singles$income.check & !is.na(singles$income.check),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(singles[singles$year == 2014 & !singles$income.check & !is.na(singles$income.check),"SLC_contribution"],contribution_bounds[,"YR2014"])]
			singles[singles$year == 2015 & !singles$income.check & !is.na(singles$income.check),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(singles[singles$year == 2015 & !singles$income.check & !is.na(singles$income.check),"SLC_contribution"],contribution_bounds[,"YR2015"])]
			singles[singles$year == 2016 & !singles$income.check & !is.na(singles$income.check),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(singles[singles$year == 2016 & !singles$income.check & !is.na(singles$income.check),"SLC_contribution"],contribution_bounds[,"YR2016"])]
			singles[singles$year == 2017 & !singles$income.check & !is.na(singles$income.check),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(singles[singles$year == 2017 & !singles$income.check & !is.na(singles$income.check),"SLC_contribution"],contribution_bounds[,"YR2017"])]
			singles[singles$year == 2018 & !singles$income.check & !is.na(singles$income.check),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(singles[singles$year == 2018 & !singles$income.check & !is.na(singles$income.check),"SLC_contribution"],contribution_bounds[,"YR2018"])]
			singles[singles$year == 2019 & !singles$income.check & !is.na(singles$income.check),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(singles[singles$year == 2019 & !singles$income.check & !is.na(singles$income.check),"SLC_contribution"],contribution_bounds[,"YR2019"])]
				
			# Get the implied subsidy bracket (combine 250-300 and 300-400 brackets)
			singles[!singles$income.check & !is.na(singles$income.check),"implied_subsidy_bracket"] <- 
				singles[!singles$income.check & !is.na(singles$income.check),"subsidy_linear_piece"]
			singles[!singles$income.check & !is.na(singles$income.check) & 
				singles$implied_subsidy_bracket %in% c("300% FPL to 400% FPL","250% FPL to 300% FPL"),
					"implied_subsidy_bracket"] <- "250% FPL to 400% FPL"
				
			# Imputed household size
			singles[!singles$income.check & !is.na(singles$income.check) & 
				(singles$subsidy_fpl_bracket == singles$implied_subsidy_bracket) &
				!is.na(singles$implied_subsidy_bracket),"implied_household_size"] <- i
			
			# Let's check how well we have matched to the subsidy_fpl_bracket variable
			
			singles[!singles$income.check & !is.na(singles$income.check) & 
				(singles$subsidy_fpl_bracket == singles$implied_subsidy_bracket) &
				!is.na(singles$implied_subsidy_bracket),"income.check"] <- TRUE

				# For the 150% FPL or less and 400% FPL or less categories:
					# Adjust income check
					# Assign implied subsidy bracket
				
				assign_400orless <- which(!singles$income.check & !is.na(singles$income.check) & 
					singles$subsidy_fpl_bracket == "400% FPL or less" & singles$implied_subsidy_bracket != "400% FPL or greater")
				assign_150orless <- which(!singles$income.check & !is.na(singles$income.check) & 
					singles$subsidy_fpl_bracket == "150% FPL or less" & singles$implied_subsidy_bracket %in% c("138% FPL or less","138% FPL to 150% FPL"))
				
				singles[intersect(assign_400orless,which(singles$year == 2014)),"subsidy_linear_piece"] <- 
					income_pieces[findInterval(singles[intersect(assign_400orless,which(singles$year == 2014)),
						"SLC_contribution"],contribution_bounds[,"YR2014"])]
				singles[intersect(assign_400orless,which(singles$year == 2015)),"subsidy_linear_piece"] <- 
					income_pieces[findInterval(singles[intersect(assign_400orless,which(singles$year == 2015)),
						"SLC_contribution"],contribution_bounds[,"YR2015"])]
				singles[intersect(assign_400orless,which(singles$year == 2016)),"subsidy_linear_piece"] <- 
					income_pieces[findInterval(singles[intersect(assign_400orless,which(singles$year == 2016)),
						"SLC_contribution"],contribution_bounds[,"YR2016"])]
				singles[intersect(assign_400orless,which(singles$year == 2017)),"subsidy_linear_piece"] <- 
					income_pieces[findInterval(singles[intersect(assign_400orless,which(singles$year == 2017)),
						"SLC_contribution"],contribution_bounds[,"YR2017"])]
				singles[intersect(assign_400orless,which(singles$year == 2018)),"subsidy_linear_piece"] <- 
					income_pieces[findInterval(singles[intersect(assign_400orless,which(singles$year == 2018)),
						"SLC_contribution"],contribution_bounds[,"YR2018"])]
				singles[intersect(assign_400orless,which(singles$year == 2019)),"subsidy_linear_piece"] <- 
					income_pieces[findInterval(singles[intersect(assign_400orless,which(singles$year == 2019)),
						"SLC_contribution"],contribution_bounds[,"YR2019"])]
				
				singles[intersect(assign_150orless,which(singles$year == 2014)),"subsidy_linear_piece"] <- 
					income_pieces[findInterval(singles[intersect(assign_150orless,which(singles$year == 2014)),
						"SLC_contribution"],contribution_bounds[,"YR2014"])]
				singles[intersect(assign_150orless,which(singles$year == 2015)),"subsidy_linear_piece"] <- 
					income_pieces[findInterval(singles[intersect(assign_150orless,which(singles$year == 2015)),
						"SLC_contribution"],contribution_bounds[,"YR2015"])]
				singles[intersect(assign_150orless,which(singles$year == 2016)),"subsidy_linear_piece"] <- 
					income_pieces[findInterval(singles[intersect(assign_150orless,which(singles$year == 2016)),
						"SLC_contribution"],contribution_bounds[,"YR2016"])]				
				singles[intersect(assign_150orless,which(singles$year == 2017)),"subsidy_linear_piece"] <- 
					income_pieces[findInterval(singles[intersect(assign_150orless,which(singles$year == 2017)),
						"SLC_contribution"],contribution_bounds[,"YR2017"])]				
				singles[intersect(assign_150orless,which(singles$year == 2018)),"subsidy_linear_piece"] <- 
					income_pieces[findInterval(singles[intersect(assign_150orless,which(singles$year == 2018)),
						"SLC_contribution"],contribution_bounds[,"YR2018"])]
				singles[intersect(assign_150orless,which(singles$year == 2019)),"subsidy_linear_piece"] <- 
					income_pieces[findInterval(singles[intersect(assign_150orless,which(singles$year == 2019)),
						"SLC_contribution"],contribution_bounds[,"YR2019"])]
						
				singles[assign_400orless,"subsidy_fpl_bracket"] <- singles[assign_400orless,"subsidy_linear_piece"]
				singles[assign_150orless,"subsidy_fpl_bracket"] <- singles[assign_150orless,"subsidy_linear_piece"]
				
				singles[intersect(assign_400orless,which(singles$subsidy_fpl_bracket %in% 
					c("300% FPL to 400% FPL","250% FPL to 300% FPL"))),"subsidy_fpl_bracket"] <- "250% FPL to 400% FPL"
				
				singles[assign_400orless,"implied_household_size"] <- i
				singles[assign_150orless,"implied_household_size"] <- i
				
				singles[assign_400orless,"income.check"] <- TRUE
				singles[assign_150orless,"income.check"] <- TRUE
		}	
	
	# Flag individuals who:
		# 1) Still have 150% or less, 400% or less (89 records)
		# 2) Implied household size is 3 or more (7,314 records)
		
		singles[singles$subsidy_fpl_bracket %in% c("150% FPL or less","400% FPL or less"),"flagged"] <- TRUE
		
		singles$imputed_income_flag <- FALSE
		singles[!singles$income.check & !is.na(singles$income.check),"imputed_income_flag"] <- TRUE
		singles[singles$implied_household_size > 2,"imputed_income_flag"] <- TRUE
		
	# For individuals with reconciled income, we can obtain the exact FPL
		
		reconciled_income <- which(is.na(singles$subsidy_fpl_percent_int) & singles$income.check & 
			!is.na(singles$income.check) & !singles$flagged)
		subsidy_income_range <- which(is.na(singles$subsidy_fpl_percent_int) & 
			!singles$subsidy_fpl_bracket %in% c("400% FPL or greater"))
		singles$fpl_LB <- singles$fpl_UB <- singles$perc_LB <- singles$perc_UB <- singles$poverty_threshold <- NA
		
		# Assign subsidy linear piece to several records that were not matched above
		singles[intersect(subsidy_income_range,
			which(is.na(singles$income.check) | !singles$income.check)),"subsidy_linear_piece"] <- 
			as.character(singles[intersect(subsidy_income_range,
			which(is.na(singles$income.check) | !singles$income.check)),"subsidy_fpl_bracket"])
		prob <- table(singles[reconciled_income,"subsidy_linear_piece"])[c("250% FPL to 300% FPL","300% FPL to 400% FPL")]/	
			sum(table(singles[reconciled_income,"subsidy_linear_piece"])[c("250% FPL to 300% FPL","300% FPL to 400% FPL")])
		singles[intersect(which(singles$subsidy_fpl_bracket %in% "250% FPL to 400% FPL"),
				which(is.na(singles$income.check) | !singles$income.check)),"subsidy_linear_piece"] <- 
			sample(c("250% FPL to 300% FPL","300% FPL to 400% FPL"),
				size=length(intersect(which(singles$subsidy_fpl_bracket %in% "250% FPL to 400% FPL"),
				which(is.na(singles$income.check) | !singles$income.check))),
				prob=prob,replace=TRUE)
		
		# Income bounds for each income bracket
		
		names(income_breakpoints) <- income_pieces[1:length(income_pieces)]
		singles[subsidy_income_range,"fpl_LB"] <- 
			income_breakpoints[singles[subsidy_income_range,"subsidy_linear_piece"]]
	
		names(income_breakpoints) <- c("whatever",income_pieces[1:length(income_pieces)-1])
		singles[subsidy_income_range,"fpl_UB"] <- 
			income_breakpoints[singles[subsidy_income_range,"subsidy_linear_piece"]]
	
		# Percentage contribution bounds for each income bracket
	
		singles[intersect(subsidy_income_range,which(singles$year == 2014)),"perc_LB"] <- 
			contribution_percentages[as.character(singles[intersect(subsidy_income_range,
				which(singles$year == 2014)),"fpl_LB"]),"YR2014"]
	
		singles[intersect(subsidy_income_range,which(singles$year == 2015)),"perc_LB"] <- 
			contribution_percentages[as.character(singles[intersect(subsidy_income_range,
				which(singles$year == 2015)),"fpl_LB"]),"YR2015"]

		singles[intersect(subsidy_income_range,which(singles$year == 2016)),"perc_LB"] <- 
			contribution_percentages[as.character(singles[intersect(subsidy_income_range,
				which(singles$year == 2016)),"fpl_LB"]),"YR2016"]
				
		singles[intersect(subsidy_income_range,which(singles$year == 2017)),"perc_LB"] <- 
			contribution_percentages[as.character(singles[intersect(subsidy_income_range,
				which(singles$year == 2017)),"fpl_LB"]),"YR2017"]
		
		singles[intersect(subsidy_income_range,which(singles$year == 2018)),"perc_LB"] <- 
			contribution_percentages[as.character(singles[intersect(subsidy_income_range,
				which(singles$year == 2018)),"fpl_LB"]),"YR2018"]
		
		singles[intersect(subsidy_income_range,which(singles$year == 2019)),"perc_LB"] <- 
			contribution_percentages[as.character(singles[intersect(subsidy_income_range,
				which(singles$year == 2019)),"fpl_LB"]),"YR2019"]
		
		singles[intersect(subsidy_income_range,which(singles$year == 2014)),"perc_UB"] <- 
			contribution_percentages[as.character(singles[intersect(subsidy_income_range,
				which(singles$year == 2014)),"fpl_UB"]),"YR2014"]
	
		singles[intersect(subsidy_income_range,which(singles$year == 2015)),"perc_UB"] <- 
			contribution_percentages[as.character(singles[intersect(subsidy_income_range,
				which(singles$year == 2015)),"fpl_UB"]),"YR2015"]

		singles[intersect(subsidy_income_range,which(singles$year == 2016)),"perc_UB"] <- 
			contribution_percentages[as.character(singles[intersect(subsidy_income_range,
				which(singles$year == 2016)),"fpl_UB"]),"YR2016"]
		
		singles[intersect(subsidy_income_range,which(singles$year == 2017)),"perc_UB"] <- 
			contribution_percentages[as.character(singles[intersect(subsidy_income_range,
				which(singles$year == 2017)),"fpl_UB"]),"YR2017"]
		
		singles[intersect(subsidy_income_range,which(singles$year == 2018)),"perc_UB"] <- 
			contribution_percentages[as.character(singles[intersect(subsidy_income_range,
				which(singles$year == 2018)),"fpl_UB"]),"YR2018"]
				
		singles[intersect(subsidy_income_range,which(singles$year == 2019)),"perc_UB"] <- 
			contribution_percentages[as.character(singles[intersect(subsidy_income_range,
				which(singles$year == 2019)),"fpl_UB"]),"YR2019"]
				
		# Poverty line
	
		singles[intersect(subsidy_income_range,which(singles$year == 2014)),"poverty_threshold"] <- 
			poverty_guidelines[as.character(singles[intersect(subsidy_income_range,which(singles$year == 2014)),"implied_household_size"]),"YR2014"]
		
		singles[intersect(subsidy_income_range,which(singles$year == 2015)),"poverty_threshold"] <- 
			poverty_guidelines[as.character(singles[intersect(subsidy_income_range,which(singles$year == 2015)),"implied_household_size"]),"YR2015"]
		
		singles[intersect(subsidy_income_range,which(singles$year == 2016)),"poverty_threshold"] <- 
			poverty_guidelines[as.character(singles[intersect(subsidy_income_range,which(singles$year == 2016)),"implied_household_size"]),"YR2016"]
		
		singles[intersect(subsidy_income_range,which(singles$year == 2017)),"poverty_threshold"] <- 
			poverty_guidelines[as.character(singles[intersect(subsidy_income_range,which(singles$year == 2017)),"implied_household_size"]),"YR2017"]
		
		singles[intersect(subsidy_income_range,which(singles$year == 2018)),"poverty_threshold"] <- 
			poverty_guidelines[as.character(singles[intersect(subsidy_income_range,which(singles$year == 2018)),"implied_household_size"]),"YR2018"]
		
		singles[intersect(subsidy_income_range,which(singles$year == 2019)),"poverty_threshold"] <- 
			poverty_guidelines[as.character(singles[intersect(subsidy_income_range,which(singles$year == 2019)),"implied_household_size"]),"YR2019"]
		
		
		# Invert the ACA subsidy formula to get FPL 
			# Turns out it is a quadratic function where the positive root is the one we need)
			# We have to do 300 to 400% of FPL separately b/c the contribution percentage is constant in this range
		
		get_exact_fpl <- function(data) {			
			a <- data$poverty_threshold/12 * (data$perc_UB - data$perc_LB)/(data$fpl_UB - data$fpl_LB)
			b <- data$poverty_threshold/12 * (data$perc_LB - (data$perc_UB - data$perc_LB) * data$fpl_LB/(data$fpl_UB - data$fpl_LB))
			c <- -data$SLC_contribution
			
			exact_fpl <- (-b + sqrt(b^2 - 4 * a * c))/(2*a)
			return(exact_fpl * 100)
		}
	
		get_exact_fpl_300to400 <- function(data) {
			exact_fpl <- data$SLC_contribution/(data$perc_LB * data$poverty_threshold/12)
			return(exact_fpl * 100)
		}
	
		singles[intersect(reconciled_income,which(singles$subsidy_linear_piece != "300% FPL to 400% FPL")),"subsidy_fpl_percent_int"] <- 
			get_exact_fpl(singles[intersect(reconciled_income,which(singles$subsidy_linear_piece != "300% FPL to 400% FPL")),])
	
		singles[intersect(reconciled_income,which(singles$subsidy_linear_piece == "300% FPL to 400% FPL")),"subsidy_fpl_percent_int"] <- 
			get_exact_fpl_300to400(singles[intersect(reconciled_income,which(singles$subsidy_linear_piece == "300% FPL to 400% FPL")),])
	
		singles[reconciled_income,"subsidy_eligible"] <- "Subsidy Eligible"	
	
	# Remaining records (311,000):
		# Group 1: 3,629 records that could not be matched above  and have enough info to determine subsidy bracket
		# Group 2: 7,400 records don't have enough information to determine subsidy bracket
		# Group 4: 300,000 records with income above 400% of FPL
	
		group1 <- which(!singles$income.check & !is.na(singles$income.check) &
					!singles$subsidy_fpl_bracket %in% c("150% FPL or less","400% FPL or less"))
		group4 <- which(is.na(singles$subsidy_fpl_percent_int) & 
			singles$subsidy_fpl_bracket == "400% FPL or greater")
		group2 <- setdiff(which(!singles$income.check | is.na(singles$income.check)),c(group1,group4))
	
		# Just flag group 2
		singles[group2,"flagged"] <- TRUE
	
		# Reset implied household size
		singles[c(group1,group2,group4),"implied_household_size"] <- 1
	
		# Group 1: randomly sample from the reconciled observations in their subsidy bracket
			# NOTE: sample the observations in each income range and possibly add small perturbation?
	
			for(i in income_pieces[1:(length(income_pieces)-1)]) {
				indices.to.simulate <- intersect(group1,which(singles$subsidy_linear_piece == i))
				sampling_distribution <- singles[intersect(reconciled_income,
					which(singles$subsidy_linear_piece == i)),"subsidy_fpl_percent_int"]
				singles[indices.to.simulate,"subsidy_fpl_percent_int"] <- 
					sample(sampling_distribution,size=length(indices.to.simulate),replace=TRUE) 
			}
			
			# Function to calculate ACA contribution
			# NOTE: this is basically the forward direction of the get_exact_fpl function above
			
			calculate_contribution <- function(data) {
				contribution <- ((data$perc_UB - data$perc_LB) * 
								(data$subsidy_fpl_percent_int/100 - data$fpl_LB)/
								(data$fpl_UB - data$fpl_LB) + data$perc_LB) * 
									(data$poverty_threshold/12 * data$subsidy_fpl_percent_int/100)
				return(contribution)
			}

			calculate_contribution_300to400 <- function(data) {
				contribution <- data$perc_LB * 
					(data$poverty_threshold/12 * data$subsidy_fpl_percent_int/100)
				return(contribution)
			}
			
			singles[intersect(group1,
					which(singles$subsidy_linear_piece != "300% FPL to 400% FPL")),"SLC_contribution"] <- 
				calculate_contribution(singles[intersect(group1,
					which(singles$subsidy_linear_piece != "300% FPL to 400% FPL")),])
			singles[intersect(group1,
					which(singles$subsidy_linear_piece == "300% FPL to 400% FPL")),"SLC_contribution"] <- 
				calculate_contribution_300to400(singles[intersect(group1,
					which(singles$subsidy_linear_piece == "300% FPL to 400% FPL")),])
			singles[group1,"subsidy"] <- pmax(0,singles[group1,"premiumSLC"] - 
				singles[group1,"SLC_contribution"])
			singles[group1,"net_premium_amt_int"] <- 
				pmax(0,singles[group1,"gross_premium_amt_int"] - 
					singles[group1,"subsidy"])
			singles[group1,"subsidy_eligible"] <- "Subsidy Eligible"
			singles[group1,"imputed_income_flag"] <- TRUE
		
		# Group 4

		singles[group4,"SLC_contribution"] <- NA	
		singles[group4,"subsidy"] <- 0
		singles[group4,"net_premium_amt_int"] <- singles[group4,"gross_premium_amt_int"] 
		singles[group4,"subsidy_eligible"] <- "Not Subsidy Elig"
		
	# Need a check on catastrophic plans (gross premium = net premium)
	singles[singles$metal_level_enhanced == "Minimum Coverage","net_premium_amt_int"] <- 
		singles[singles$metal_level_enhanced == "Minimum Coverage","gross_premium_amt_int"] 
		
	# Port over variables to main data object	
	import_variables <- c("gross_premium_amt_int","net_premium_amt_int","subsidy_fpl_bracket",
		"subsidy_fpl_percent_int","subsidy_eligible","subsidy","age","flagged")
	new_variables <- c("premiumSLC","SLC_contribution","implied_household_size",
						"subsidy_linear_piece","imputed_income_flag","rating_factor","zero_premium")
	
	data[,new_variables] <- NA
	for(variable in new_variables) {
		data[rownames(singles),variable] <- singles[,variable] 
	}
	
	for(variable in import_variables) {
		data[rownames(singles),variable] <- singles[,variable] 
	}
	
	# Save the data object 
	save(data,file="data/final/enroll_temp")	
	gc()
	
	
	
##### Multi-Person Households - Net Premium and Income Reconciliation	 
	
	# Reload data
	data <- get(load("data/final/enroll_temp"))
	plan_data <- read.csv("data/plan_data.csv") # Covered California plan data
	age_rating_factors <- read.csv("data/age_rating_factors.csv",row.names=1) # CCIIO default rating curve
	poverty_guidelines <- read.csv("data/poverty_guidelines.csv",row.names=1) # Poverty guidelines
	contribution_percentages <- read.csv("data/contribution_percentages.csv",row.names = 1) # ACA contribution percentages
	rating_areas <- read.csv("data/rating_areas.csv",row.names = 1) # California county-rating area mapping
	zip3_choices <- read.csv("data/zip3_choices.csv",row.names = 1) # choice set by 3 digit zip and rating area
	product_definitions <- read.csv("data/product_definitions.csv",row.names = 1) # definitions of column names in zip3_choices
	
	set.seed(6)
	
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
	
	
	# Compute Summary Statistics
	
		compute.number.unique <- function(x) {
			return(length(unique(x)))
		}
	
		# Members per household
		members_per_household <- by(data$individual_id,data$household_year,length)	
		plans_per_household <- by(data$plan_name,data$household_year,compute.number.unique)
		flagged_household <- by(data$flagged,data$household_year,sum)
	
		# Ages
		
		data[is.na(data$age) | data$age > 120,"flagged"] <- TRUE
		data[is.na(data$age) | data$age > 120,"age"] <- NA
		
		number_0to17 <- by(data[data$age < 18 & !is.na(data$age),"age"],
			data[data$age < 18 & !is.na(data$age),"household_year"],length)
		number_18to25 <- by(data[data$age >= 18 & data$age < 26 & !is.na(data$age),"age"],
			data[data$age >= 18 & data$age < 26 & !is.na(data$age),"household_year"],length)
		number_26to34 <- by(data[data$age >= 26 & data$age < 35 & !is.na(data$age),"age"],
			data[data$age >= 26 & data$age < 35 & !is.na(data$age),"household_year"],length)
		number_35to44 <- by(data[data$age >= 35 & data$age < 45 & !is.na(data$age),"age"],
			data[data$age >= 35 & data$age < 45 & !is.na(data$age),"household_year"],length)
		number_45to54 <- by(data[data$age >= 45 & data$age < 55 & !is.na(data$age),"age"],
			data[data$age >= 45 & data$age < 55 & !is.na(data$age),"household_year"],length)
		number_55to64 <- by(data[data$age >= 55 & data$age < 65 & !is.na(data$age),"age"],
			data[data$age >= 55 & data$age < 65 & !is.na(data$age),"household_year"],length)
		number_65plus <- by(data[data$age >= 65 & !is.na(data$age),"age"],
			data[data$age >= 65 & !is.na(data$age),"household_year"],length)
	
	# Build Household Object
	
	households <- as.data.frame(cbind(members_per_household,plans_per_household,
		as.numeric(flagged_household > 0)))
	rownames(households) <- names(members_per_household)
	colnames(households) <- c("members","plans","flagged")
	
	households$number_0to17 <- households$number_18to25 <- households$number_26to34 <-  households$number_35to44 <- 
		households$number_45to54 <- households$number_55to64 <- households$number_65plus <- 0
	
	households[names(number_0to17),"number_0to17"] <- number_0to17
	households[names(number_18to25),"number_18to25"] <- number_18to25
	households[names(number_26to34),"number_26to34"] <- number_26to34
	households[names(number_35to44),"number_35to44"] <- number_35to44
	households[names(number_45to54),"number_45to54"] <- number_45to54
	households[names(number_55to64),"number_55to64"] <- number_55to64
	households[names(number_65plus),"number_65plus"] <- number_65plus
	
	# Identify families
	family_households <- rownames(households[households$members > 1 & !households$flagged,])	
	
	# Add premiums
	gross_premiums <- by(data[!data$flagged,"gross_premium_amt_int"],data[!data$flagged,"household_year"],unique)
	net_premiums <- by(data[!data$flagged,"net_premium_amt_int"],data[!data$flagged,"household_year"],unique)
	
	households$gross_premium_amt_int <- households$net_premium_amt_int <- NA
	households[names(gross_premiums),"gross_premium_amt_int"] <- pmax(0,gross_premiums)
	households[names(net_premiums),"net_premium_amt_int"] <- pmax(0,net_premiums)

	# 21-year old single premium for chosen plan
		# NOTE: The premium would be different for multi-plan households, but there are only 60 
		# non-flagged of these, and hence too trivial a number to worry about
	
	pull_first <- function(x) {
		return(x[1])
	}
	
	unique_premium21 <- by(data[data$household_year %in% family_households & !data$flagged,"premium21"],
		data[data$household_year %in% family_households & !data$flagged,"household_year"],pull_first)
	households$premium21 <- NA
	households[names(unique_premium21),"premium21"] <- unique_premium21
			
	# Keep track of households receiving zero subsidy
			
	households$zero_subsidy <- NA
	households[family_households,"zero_subsidy"] <- 
		as.numeric(households[family_households,"gross_premium_amt_int"] == 
			households[family_households,"net_premium_amt_int"] & 
		households[family_households,"gross_premium_amt_int"] > 0)
		
	### This section reconciles the net premium and income
		# There should only be one of each per household
		# There could be both subsidized and unsubsidized members, but should be consistent with net premium and income

	family_households <- rownames(households[households$members > 1 & !households$flagged,])	
	family_indices <- which(households$members > 1 & !households$flagged)
	families <- data[data$household_year %in% family_households,]

	# Variables we need to populate
	households$premiumSLC <- NA
	households$subsidy <- NA
	households$SLC_contribution <- NA
	
	# Compute second-lowest silver premium 
		# There should be one SLC plan in each rating area for each plan year
		# We can do this because we now have age
			
	get_available_silver_plans <- function(i,zip3_choices) {
		
			region_year_plans <- which(plan_data$ENROLLMENT_YEAR == zip3_choices[i,"Year"] &
				plan_data$region == zip3_choices[i,"Region"] & plan_data$metal_level == "Silver")
		
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
			return(available_plans)
		}
		
		compute_second_lowest <- function(i,zip3_choices) {
		
			# Get available silver plans
			available_plans <- get_available_silver_plans(i,zip3_choices)
			silver_premiums <- sort(plan_data[available_plans,"Premium"])/1.278
				
			if(length(silver_premiums) == 1) {
				return(silver_premiums[1])
			} else {
				return(silver_premiums[2])
			}
		}	
	
	single_product_insurers <- c("Chinese_Community","Contra_Costa","Kaiser","LA_Care","Molina",
				"Oscar","United","Valley","Western")
	zipchoices <- as.matrix(zip3_choices[,4:ncol(zip3_choices)])
	zip3_choices$premiumSLC <- sapply(rownames(zip3_choices),FUN=compute_second_lowest,zip3_choices)
			
	families$rating_factor <- age_rating_factors[as.character(pmin(64,families$age)),"Rating_Factor"]
	families[families$year >= 2018,"rating_factor"] <- 
		age_rating_factors[as.character(pmin(64,families[families$year >= 2018,"age"])),"Rating_Factor2018"]
		
	families$premiumSLC <- zip3_choices[families$zip_region_year,"premiumSLC"] * families$rating_factor
	households[family_households,"premiumSLC"] <- 
		by(families$premiumSLC,families$household_year,sum)[family_households]
	
	# Compute subsidy
	households[family_indices,"subsidy"] <- 
		pmax(0,households[family_indices,"gross_premium_amt_int"] - 
			households[family_indices,"net_premium_amt_int"]) 
	
	# Compute family's required contribution towards SLC silver plan
	households[family_indices,"SLC_contribution"] <- 
		pmax(0,households[family_indices,"premiumSLC"] - households[family_indices,"subsidy"]) 
	
	# Determine subsidy eligibility composition of household
		
		number_subsidized <- function(x) {
			return(length(x[x == "Subsidy Eligible"]))
		}
			
		number_unsubsidized <- function(x) {
			return(length(x[x == "Not Subsidy Elig"]))
		}	
			
		households$subsidized_members <- NA
		households$unsubsidized_members <- NA
		
		subsidized_members <- by(families$subsidy_eligible,families$household_year,number_subsidized)
		unsubsidized_members <- by(families$subsidy_eligible,families$household_year,number_unsubsidized)
		
		households[names(subsidized_members),"subsidized_members"] <- subsidized_members
		households[names(unsubsidized_members),"unsubsidized_members"] <- unsubsidized_members	
	
	#Import subsidy bracket into household object
		# There are still about 3,000 households with multiple subsidy brackets	
			# We'll try two different brackets when reconciling
		
	pull_first <- function(x) {
		return(as.character(x[1]))
	}
		
	households$subsidy_fpl_bracket <- NA
	households[family_households,"subsidy_fpl_bracket"] <- 
		by(families$subsidy_fpl_bracket,families$household_year,pull_first)[family_households]
		
	households$FPL <- NA	
	households[family_households,"FPL"] <- 
		as.integer(by(families$subsidy_fpl_percent_int,
		families$household_year,pull_first)[family_households])
	
	# Import enrollment year
	households$year <- NA
	households[family_households,"year"] <- 
		by(families$year,families$household_year,unique)[family_households]
	
	# Keep track of mixed subsidy households
		# There are 7847 of these households
		# See if you can reconcile these below assuming all are subsidized; otherwise, flag
	
	mixed_subsidy_households <- which(households$unsubsidized_members > 0 & 
		households$subsidized_members > 0 & !is.na(households$subsidized_members))
	
	# There is a major problem here:
		# Some people have effectively zero premium (I believe each person has to pay at least $1)
		# People use their subsidies to buy a bronze plan (or the lowest-cost silver plan)
		# In many cases, the subsidy based on SLC is bigger than the premium, so it ends up getting reduced
		# So the smaller subsidy leads to a larger SLC contribution, which leads to higher estimated income, which
		# will inevitably lead to higher estimated premiums faced by the household.
		
		# Solution: let's reconcile below without imputing household size for these people
		# This income level is an upper bound on income.  It may not even be a tight upper bound
		# if it is above the household's income range 	
		
	threshold_for_zero <- 10
	households$zero_premium <- FALSE
	households[households$net_premium_amt_int <= threshold_for_zero & 
		!is.na(households$net_premium_amt_int),"zero_premium"] <- TRUE 
		
	# Determine income piece to search
	
	income_pieces <- c("138% FPL or less","138% FPL to 150% FPL","150% FPL to 200% FPL",
			"200% FPL to 250% FPL","250% FPL to 300% FPL","300% FPL to 400% FPL","400% FPL or greater")
	income_breakpoints <- c(0,1.38,1.5,2,2.5,3,4)
	max_imputation_size <- 5
		
	households$subsidy_linear_piece <- NA
	households$implied_subsidy_bracket <- NA
	households$implied_household_size <- households$members
			
	# Income check dummy (NA for those receiving no subsidy or those for whom the subsidy is NA)
	households$income.check <- FALSE
	households[!is.na(households$FPL),"income.check"] <- TRUE
	households[households$subsidy == 0 | is.na(households$subsidy),"income.check"] <- NA
	
	check_income_match <- function(households) {
		
		# Get the implied subsidy bracket (combine 250-300 and 300-400 brackets)
		households$implied_subsidy_bracket <- households$subsidy_linear_piece
		combine_incomes <- which(households$implied_subsidy_bracket %in% 
				c("300% FPL to 400% FPL","250% FPL to 300% FPL"))
		if(length(combine_incomes) > 0) { 
			households[combine_incomes,"implied_subsidy_bracket"] <- "250% FPL to 400% FPL"
		}
			
		# Let's check how well we have matched to the subsidy_fpl_bracket variable
		households[which(households[,"subsidy_fpl_bracket"] == 
			households[,"implied_subsidy_bracket"]),"income.check"] <- TRUE				
		
		# For the 150% FPL or less and 400% FPL or less categories:
			# Adjust income check
			# Assign implied subsidy bracket
				
			assign_400orless <- which(!households$income.check & !is.na(households$income.check) & 
				households$subsidy_fpl_bracket == "400% FPL or less" & households$implied_subsidy_bracket != "400% FPL or greater")
			assign_150orless <- which(!households$income.check & !is.na(households$income.check) & 
				households$subsidy_fpl_bracket == "150% FPL or less" & households$implied_subsidy_bracket %in% c("138% FPL or less","138% FPL to 150% FPL"))
			
			households[intersect(assign_400orless,which(households$year == 2014)),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(assign_400orless,which(households$year == 2014)),"SLC_contribution"],contribution_bounds[,"YR2014"])]
			households[intersect(assign_400orless,which(households$year == 2015)),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(assign_400orless,which(households$year == 2015)),"SLC_contribution"],contribution_bounds[,"YR2015"])]
			households[intersect(assign_400orless,which(households$year == 2016)),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(assign_400orless,which(households$year == 2016)),"SLC_contribution"],contribution_bounds[,"YR2016"])]
			households[intersect(assign_400orless,which(households$year == 2017)),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(assign_400orless,which(households$year == 2017)),"SLC_contribution"],contribution_bounds[,"YR2017"])]
			households[intersect(assign_400orless,which(households$year == 2018)),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(assign_400orless,which(households$year == 2018)),"SLC_contribution"],contribution_bounds[,"YR2018"])]
			households[intersect(assign_400orless,which(households$year == 2019)),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(assign_400orless,which(households$year == 2019)),"SLC_contribution"],contribution_bounds[,"YR2019"])]
			
			households[intersect(assign_150orless,which(households$year == 2014)),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(assign_150orless,which(households$year == 2014)),"SLC_contribution"],contribution_bounds[,"YR2014"])]
			households[intersect(assign_150orless,which(households$year == 2015)),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(assign_150orless,which(households$year == 2015)),"SLC_contribution"],contribution_bounds[,"YR2015"])]
			households[intersect(assign_150orless,which(households$year == 2016)),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(assign_150orless,which(households$year == 2016)),"SLC_contribution"],contribution_bounds[,"YR2016"])]
			households[intersect(assign_150orless,which(households$year == 2017)),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(assign_150orless,which(households$year == 2017)),"SLC_contribution"],contribution_bounds[,"YR2017"])]
			households[intersect(assign_150orless,which(households$year == 2018)),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(assign_150orless,which(households$year == 2018)),"SLC_contribution"],contribution_bounds[,"YR2018"])]
			households[intersect(assign_150orless,which(households$year == 2019)),"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(assign_150orless,which(households$year == 2019)),"SLC_contribution"],contribution_bounds[,"YR2019"])]
			
			households[assign_400orless,"subsidy_fpl_bracket"] <- households[assign_400orless,"subsidy_linear_piece"]
			households[assign_150orless,"subsidy_fpl_bracket"] <- households[assign_150orless,"subsidy_linear_piece"]
			
			households[intersect(assign_400orless,which(households$subsidy_fpl_bracket %in% 
				c("300% FPL to 400% FPL","250% FPL to 300% FPL"))),"subsidy_fpl_bracket"] <- "250% FPL to 400% FPL"
			
			households[assign_400orless,"income.check"] <- TRUE
			households[assign_150orless,"income.check"] <- TRUE					
		
		return(households)
	}
	
	for (s in 2:max(households$members)) {
		
		contribution_bounds <- cbind(contribution_percentages[,"YR2014"] * poverty_guidelines[s,"YR2014"],
											contribution_percentages[,"YR2015"] * poverty_guidelines[s,"YR2015"],
											contribution_percentages[,"YR2016"] * poverty_guidelines[s,"YR2016"],
											contribution_percentages[,"YR2017"] * poverty_guidelines[s,"YR2017"],
											contribution_percentages[,"YR2018"] * poverty_guidelines[s,"YR2018"],
											contribution_percentages[,"YR2019"] * poverty_guidelines[s,"YR2019"])/12 * income_breakpoints
		colnames(contribution_bounds) <- c("YR2014","YR2015","YR2016","YR2017","YR2018","YR2019")
			
		
		households_to_process <- intersect(family_indices,
			which(households$implied_household_size == s & 
				!households$income.check & !is.na(households$income.check)))
			
		if(length(households_to_process) > 0) { 	
			
			# Get linear piece
			households[intersect(households_to_process,which(households$year == 2014)),
				"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(households_to_process,
				which(households$year == 2014)),"SLC_contribution"],
				contribution_bounds[,"YR2014"])]
			households[intersect(households_to_process,which(households$year == 2015)),
				"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(households_to_process,
				which(households$year == 2015)),"SLC_contribution"],
				contribution_bounds[,"YR2015"])]
			households[intersect(households_to_process,which(households$year == 2016)),
				"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(households_to_process,
				which(households$year == 2016)),"SLC_contribution"],
				contribution_bounds[,"YR2016"])]
			households[intersect(households_to_process,which(households$year == 2017)),
				"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(households_to_process,
				which(households$year == 2017)),"SLC_contribution"],
				contribution_bounds[,"YR2017"])]
			households[intersect(households_to_process,which(households$year == 2018)),
				"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(households_to_process,
				which(households$year == 2018)),"SLC_contribution"],
				contribution_bounds[,"YR2018"])]
			households[intersect(households_to_process,which(households$year == 2019)),
				"subsidy_linear_piece"] <- 
				income_pieces[findInterval(households[intersect(households_to_process,
				which(households$year == 2019)),"SLC_contribution"],
				contribution_bounds[,"YR2019"])]
				
			# Check how well we have matched
			households[households_to_process,] <- check_income_match(households[households_to_process,])			
		}
			
		# As long as the implied household size is below a certain level, keep trying to match
		
		households_to_process <- intersect(family_indices,
			which(households$implied_household_size == s & 
			!households$income.check & !is.na(households$income.check)))
		
		if(s < max_imputation_size) {
			households[households_to_process,"implied_household_size"] <- 
				households[households_to_process,"implied_household_size"] + 1
		}
	}
	
	# Flag individuals who:
		# 0) Unreconciled mixed subsidy households(3 records)
		# 1) Still have 150% or less, 400% or less (4300 records)
		# 2) Implied household size is 3 or more (40600 records)
		# 3) Failed the income check
		
		###### NOTE: the number of < 150 or less households that get flagged here is 1,100 or so
		###### This is quite high.  All have implied contributions that are much higher than what
		###### someone at this income level would be expected to contribute.  
		
	households[intersect(mixed_subsidy_households,which(!households$income.check & 
		!is.na(households$income.check))),"flagged"] <- TRUE	
	households[intersect(mixed_subsidy_households,which(households$income.check & 
			!is.na(households$income.check))),"subsidized_members"] <- 
		households[intersect(mixed_subsidy_households,which(households$income.check & 
			!is.na(households$income.check))),"members"] 
	households[intersect(mixed_subsidy_households,which(households$income.check & 
		!is.na(households$income.check))),"unsubsidized_members"] <- 0
	data[data$household_year %in% intersect(mixed_subsidy_households,
		which(households$income.check & !is.na(households$income.check))),
		"subsidy_eligible"] <- "Subsidy Eligible"
	
	households[intersect(family_indices,which(households$subsidy_fpl_bracket %in% 
		c("150% FPL or less","400% FPL or less"))),"flagged"] <- TRUE
		
	households$imputed_income_flag <- FALSE
	households[intersect(family_indices,which(!households$income.check & 
		!is.na(households$income.check))),"imputed_income_flag"] <- TRUE
	households[intersect(family_indices,which(households$implied_household_size - 
		households$members > 2)),"imputed_income_flag"] <- TRUE
	
	# As a check, make sure that no households w/ bracket 400% or greater were reconciled above
	households[intersect(family_indices,
		which(households$subsidy_fpl_bracket == "400% FPL or greater")),
		c("subsidy","income.check","subsidy_linear_piece")] <- NA
	households[intersect(family_indices,
		which(households$subsidy_fpl_bracket == "400% FPL or greater")),
			"net_premium_amt_int"] <- 
	households[intersect(family_indices,
		which(households$subsidy_fpl_bracket == "400% FPL or greater")),
			"gross_premium_amt_int"] 
	households[intersect(family_indices,
		which(households$subsidy_fpl_bracket == "400% FPL or greater")),
			"subsidized_members"] <- 0
	households[intersect(family_indices,
		which(households$subsidy_fpl_bracket == "400% FPL or greater")),
			"unsubsidized_members"] <- 
	households[intersect(family_indices,
		which(households$subsidy_fpl_bracket == "400% FPL or greater")),"members"]
	
	# For individuals with reconciled income, we can obtain the exact FPL	
		
		reconciled_income <- intersect(family_indices,
			which(households$income.check & !is.na(households$income.check) & 
			!households$flagged & is.na(households$FPL)))
		subsidy_income_range <- intersect(family_indices,
			which(!households$subsidy_fpl_bracket %in% c("400% FPL or greater") & 
			is.na(households$FPL)))
		
		subsidy_variables <- c("fpl_LB","fpl_UB","perc_LB","perc_UB")
		households[,subsidy_variables] <- NA
		
		# Assign subsidy linear piece to the households that were not matched above
		households[setdiff(subsidy_income_range,reconciled_income),"subsidy_linear_piece"] <- 
			as.character(households[setdiff(subsidy_income_range,reconciled_income),"subsidy_fpl_bracket"])
		prob <- table(households[reconciled_income,"subsidy_linear_piece"])[c("250% FPL to 300% FPL","300% FPL to 400% FPL")]/	
			sum(table(households[reconciled_income,"subsidy_linear_piece"])[c("250% FPL to 300% FPL","300% FPL to 400% FPL")])
		households[intersect(setdiff(subsidy_income_range,reconciled_income),
			which(households$subsidy_fpl_bracket %in% "250% FPL to 400% FPL")),"subsidy_linear_piece"] <- 
			sample(c("250% FPL to 300% FPL","300% FPL to 400% FPL"),
				size=length(intersect(setdiff(subsidy_income_range,reconciled_income),
				which(households$subsidy_fpl_bracket %in% "250% FPL to 400% FPL"))),
				prob=prob,replace=TRUE)
		
		# Income bounds for each income bracket
		
		names(income_breakpoints) <- income_pieces[1:length(income_pieces)]
		households[subsidy_income_range,"fpl_LB"] <- 
			income_breakpoints[households[subsidy_income_range,"subsidy_linear_piece"]]
	
		names(income_breakpoints) <- c("whatever",income_pieces[1:length(income_pieces)-1])
		households[subsidy_income_range,"fpl_UB"] <- 
			income_breakpoints[households[subsidy_income_range,"subsidy_linear_piece"]]
	
		# Percentage contribution bounds for each income bracket
	
		households[intersect(subsidy_income_range,
				which(households$year == 2014)),"perc_LB"] <- 
			contribution_percentages[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2014)),"fpl_LB"]),"YR2014"]
	
		households[intersect(subsidy_income_range,
				which(households$year == 2015)),"perc_LB"] <- 
			contribution_percentages[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2015)),"fpl_LB"]),"YR2015"]
	
		households[intersect(subsidy_income_range,
				which(households$year == 2016)),"perc_LB"] <- 
			contribution_percentages[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2016)),"fpl_LB"]),"YR2016"]	

		households[intersect(subsidy_income_range,
				which(households$year == 2017)),"perc_LB"] <- 
			contribution_percentages[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2017)),"fpl_LB"]),"YR2017"]

		households[intersect(subsidy_income_range,
				which(households$year == 2018)),"perc_LB"] <- 
			contribution_percentages[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2018)),"fpl_LB"]),"YR2018"]
		
		households[intersect(subsidy_income_range,
				which(households$year == 2019)),"perc_LB"] <- 
			contribution_percentages[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2019)),"fpl_LB"]),"YR2019"]
		
		households[intersect(subsidy_income_range,
				which(households$year == 2014)),"perc_UB"] <- 
			contribution_percentages[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2014)),"fpl_UB"]),"YR2014"]
	
		households[intersect(subsidy_income_range,
				which(households$year == 2015)),"perc_UB"] <- 
			contribution_percentages[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2015)),"fpl_UB"]),"YR2015"]

		households[intersect(subsidy_income_range,
				which(households$year == 2016)),"perc_UB"] <- 
			contribution_percentages[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2016)),"fpl_UB"]),"YR2016"]
				
		households[intersect(subsidy_income_range,
				which(households$year == 2017)),"perc_UB"] <- 
			contribution_percentages[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2017)),"fpl_UB"]),"YR2017"]
				
		households[intersect(subsidy_income_range,
				which(households$year == 2018)),"perc_UB"] <- 
			contribution_percentages[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2018)),"fpl_UB"]),"YR2018"]
			
		households[intersect(subsidy_income_range,
				which(households$year == 2019)),"perc_UB"] <- 
			contribution_percentages[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2019)),"fpl_UB"]),"YR2019"]
			
		# Poverty line
	
		households[intersect(subsidy_income_range,
				which(households$year == 2014)),"poverty_threshold"] <- 
			poverty_guidelines[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2014)),"implied_household_size"]),"YR2014"]
		
		households[intersect(subsidy_income_range,
				which(households$year == 2015)),"poverty_threshold"] <- 
			poverty_guidelines[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2015)),"implied_household_size"]),"YR2015"]
		
		households[intersect(subsidy_income_range,
				which(households$year == 2016)),"poverty_threshold"] <- 
			poverty_guidelines[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2016)),"implied_household_size"]),"YR2016"]
		
		households[intersect(subsidy_income_range,
				which(households$year == 2017)),"poverty_threshold"] <- 
			poverty_guidelines[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2017)),"implied_household_size"]),"YR2017"]
		
		households[intersect(subsidy_income_range,
				which(households$year == 2018)),"poverty_threshold"] <- 
			poverty_guidelines[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2018)),"implied_household_size"]),"YR2018"]
				
		households[intersect(subsidy_income_range,
				which(households$year == 2019)),"poverty_threshold"] <- 
			poverty_guidelines[as.character(households[intersect(subsidy_income_range,
				which(households$year == 2019)),"implied_household_size"]),"YR2019"]
		
		# Invert the ACA subsidy formula to get FPL 
			# Turns out it is a quadratic function where the positive root is the one we need)
			# We have to do 300 to 400% of FPL separately b/c the contribution percentage is constant in this range
		
		get_exact_fpl <- function(data) {			
			a <- data$poverty_threshold/12 * (data$perc_UB - data$perc_LB)/(data$fpl_UB - data$fpl_LB)
			b <- data$poverty_threshold/12 * (data$perc_LB - (data$perc_UB - data$perc_LB) * 
				data$fpl_LB/(data$fpl_UB - data$fpl_LB))
			c <- -data$SLC_contribution
			
			exact_fpl <- (-b + sqrt(b^2 - 4 * a * c))/(2*a)
			return(exact_fpl * 100)
		}
	
		get_exact_fpl_300to400 <- function(data) {
			exact_fpl <- data$SLC_contribution/(data$perc_LB * data$poverty_threshold/12)
			return(exact_fpl * 100)
		}
	
		households[intersect(reconciled_income,
				which(households$subsidy_linear_piece != "300% FPL to 400% FPL")),"FPL"] <- 
			get_exact_fpl(households[intersect(reconciled_income,
				which(households$subsidy_linear_piece != "300% FPL to 400% FPL")),])
	
		households[intersect(reconciled_income,
				which(households$subsidy_linear_piece == "300% FPL to 400% FPL")),"FPL"] <- 
			get_exact_fpl_300to400(households[intersect(reconciled_income,
				which(households$subsidy_linear_piece == "300% FPL to 400% FPL")),])
	

	# Remaining households (233,000 records):
		# Group 1: 29,000 records that could not be matched above  and have enough info to determine subsidy bracket
		# Group 2: 66,000 records don't have enough information to determine subsidy bracket
		# Group 4: 138,000 records with income above 400% of FPL
	
		group1 <- intersect(family_indices,
			which(!households$income.check & !is.na(households$income.check) &
					!households$subsidy_fpl_bracket %in% c("150% FPL or less","400% FPL or less")))
		group4 <- intersect(family_indices,
			which((!households$income.check | is.na(households$income.check))
				& households$subsidy_fpl_bracket == "400% FPL or greater"))
		group2 <- intersect(family_indices,
			setdiff(which(!households$income.check | is.na(households$income.check)),c(group1,group4)))
	
		
		# Reset implied household size
		households[c(group1,group2,group4),"implied_household_size"] <- 
			households[c(group1,group2,group4),"members"]
	
		# Just flag group 2
		households[group2,"flagged"] <- TRUE
	
		# Group 1: randomly sample from the reconciled observations in their subsidy bracket
			# NOTE: sample the observations in each income range and possibly add small perturbation?
			# NOTE; I really should use the broader population here
	
			for(i in income_pieces[1:(length(income_pieces)-1)]) {
				households.to.simulate <- intersect(group1,
					which(households$subsidy_linear_piece == i))
				sampling_distribution <- 
					households[which(households$subsidy_linear_piece == i & !is.na(households$FPL)),"FPL"]
				households[households.to.simulate,"FPL"] <- 
					sample(sampling_distribution,size=length(households.to.simulate),replace=TRUE) 
			}
			
			# Function to calculate ACA contribution
				# NOTE: this is basically the forward direction of the get_exact_fpl function above
			
			calculate_contribution <- function(data) {
				contribution <- ((data$perc_UB - data$perc_LB) * (data$FPL/100 - data$fpl_LB)/
								(data$fpl_UB - data$fpl_LB) + data$perc_LB) * 
									(data$poverty_threshold/12 * data$FPL/100)
				return(contribution)
			}

			calculate_contribution_300to400 <- function(data) {
				contribution <- data$perc_LB * (data$poverty_threshold/12 * data$FPL/100)
				return(contribution)
			}
			
			households[intersect(group1,
					which(households$subsidy_linear_piece != "300% FPL to 400% FPL")),"SLC_contribution"] <- 
				calculate_contribution(households[intersect(group1,
					which(households$subsidy_linear_piece != "300% FPL to 400% FPL")),])
			households[intersect(group1,
					which(households$subsidy_linear_piece == "300% FPL to 400% FPL")),"SLC_contribution"] <- 
				calculate_contribution_300to400(households[intersect(group1,
					which(households$subsidy_linear_piece == "300% FPL to 400% FPL")),])
			households[group1,"subsidy"] <- pmax(0,households[group1,"premiumSLC"] - 
				households[group1,"SLC_contribution"])
			households[group1,"net_premium_amt_int"] <- 
				pmax(0,households[group1,"gross_premium_amt_int"] - households[group1,"subsidy"])
			
			# NOTE the addition of the unsubsidized premium portion to the net premium - you need to keep this in mind for later simulations
			
			households[group1,"imputed_income_flag"] <- TRUE
		
			
		# Group 4

		households[group4,"SLC_contribution"] <- NA	
		households[group4,"subsidy"] <- 0
		households[group4,"net_premium_amt_int"] <- households[group4,"gross_premium_amt_int"] 
		households[group4,"subsidized_members"] <- 0
		households[group4,"unsubsidized_members"] <- households[group4,"members"] 
		
		# Save the data objects 
		save(families,file="data/final/family_enroll_temp")	
		save(households,file="data/final/household_temp")	
		gc()
		
##### Final Data Cleanup and Import

	data <- get(load("data/final/enroll_temp"))
	families <- get(load("data/final/family_enroll_temp"))
	households <- get(load("data/final/household_temp"))
	plan_data <- read.csv("data/plan_data.csv") # Covered California plan data
	age_rating_factors <- read.csv("data/age_rating_factors.csv",row.names=1) # CCIIO default rating curve
	poverty_guidelines <- read.csv("data/poverty_guidelines.csv",row.names=1) # Poverty guidelines
	contribution_percentages <- read.csv("data/contribution_percentages.csv",row.names = 1) # ACA contribution percentages
	rating_areas <- read.csv("data/rating_areas.csv",row.names = 1) # California county-rating area mapping
	zip3_choices <- read.csv("data/zip3_choices.csv",row.names = 1) # choice set by 3 digit zip and rating area
	product_definitions <- read.csv("data/product_definitions.csv",row.names = 1) # definitions of column names in zip3_choices
	
	
	
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
	
	data$members <- households[data$household_year,"members"]
	
	family_indices <- which(households$members > 1 & !households$flagged)	
	single_indices <- which(households$members == 1 & !households$flagged)
	
	family_households <- rownames(households[households$members > 1 & !households$flagged,])
	single_households <- rownames(households[households$members == 1 & !households$flagged,])
	
	# Populate single households
	singles <- data[data$household_year %in%  single_households,]
	singles <- arrange(singles,household_year)
	
	households[single_indices,"premium21"] <- singles$premium21
	households[single_indices,"premiumSLC"] <- singles$premiumSLC
	households[single_indices,"subsidy"] <- singles$subsidy
	households[single_indices,"net_premium_amt_int"] <- singles$net_premium_amt_int
	households[single_indices,"gross_premium_amt_int"] <- singles$gross_premium_amt_int
	households[single_indices,"subsidized_members"] <- as.numeric(singles$subsidy_eligible == "Subsidy Eligible")
	households[single_indices,"unsubsidized_members"] <- as.numeric(singles$subsidy_eligible == "Not Subsidy Elig")
	households[single_indices,"subsidy_fpl_bracket"] <- as.character(singles$subsidy_fpl_bracket)
	households[single_indices,"year"] <- singles$year
	households[single_indices,"subsidy_linear_piece"] <- singles$subsidy_linear_piece
	households[single_indices,"implied_household_size"] <- singles$implied_household_size
	households[single_indices,"FPL"] <- singles$subsidy_fpl_percent_int
	households[single_indices,"metal_level_enhanced"] <- singles$metal_level_enhanced
	households[single_indices,"flagged"] <- singles$flagged
	households[single_indices,"imputed_income_flag"] <- singles$flagged
	
	# metal_level_enhanced is missing for family households
	pull_first <- function(x) {
		return(as.character(x[1]))
	}
	
	households_metals <- by(data$metal_level_enhanced,data$household_year,pull_first)
	households$metal_level_enhanced <- households_metals[rownames(households)]
			
	
	return_unique <- function(x) {
		return(as.character(unique(x)))
	}
	
	households[,c("region","zip3")] <- NA
	households[single_indices,c("region","zip3")] <- singles[,c("region","zip3")]
	households[family_households,"region"] <- as.integer(by(families$region,
		families$household_year,return_unique)[family_households])
	households[family_households,"zip3"] <- as.integer(by(families$zip3,
		families$household_year,return_unique)[family_households])
	
	# Verify income variables

		households[households$FPL >= 0 & households$FPL <= 138 & 
			!is.na(households$FPL),"subsidy_fpl_bracket"] <- 
			"138% FPL or less"
		households[households$FPL > 138 & households$FPL <= 150 & 
			!is.na(households$FPL),"subsidy_fpl_bracket"] <- 
			"138% FPL to 150% FPL"
		households[households$FPL > 150 & households$FPL <= 200 & 
			!is.na(households$FPL),"subsidy_fpl_bracket"] <- 
			"150% FPL to 200% FPL"
		households[households$FPL > 200 & households$FPL <= 250 & 
			!is.na(households$FPL),"subsidy_fpl_bracket"] <- 
			"200% FPL to 250% FPL"
		households[households$FPL > 250 & households$FPL <= 400 & 
			!is.na(households$FPL),"subsidy_fpl_bracket"] <- 
			"250% FPL to 400% FPL"
		households[households$FPL > 400 & !is.na(households$FPL),"subsidy_fpl_bracket"] <-
			"400% FPL or greater"
			
		households[households$subsidy_fpl_bracket == "400% FPL or greater" & 
			!is.na(households$subsidy_fpl_bracket),"subsidized_members"] <- 0  
		households[households$subsidy_fpl_bracket == "400% FPL or greater" & 
			!is.na(households$subsidy_fpl_bracket),"unsubsidized_members"] <- 
		households[households$subsidy_fpl_bracket == "400% FPL or greater"& 
			!is.na(households$subsidy_fpl_bracket), "members"]
				
		households$subsidy_linear_piece <- households$subsidy_fpl_bracket
		households[households$FPL > 300 & households$FPL <= 400 & !is.na(households$FPL),
			"subsidy_linear_piece"] <- "300% FPL to 400% FPL"
		households[households$FPL > 250 & households$FPL <= 300 & !is.na(households$FPL),
			"subsidy_linear_piece"] <- "250% FPL to 300% FPL"
		households[is.na(households$FPL),"subsidy_linear_piece"] <- NA
		households[households$subsidized_members == 0 & !is.na(households$subsidized_members),
			"subsidy_linear_piece"] <- NA

	# Now we need to check that all of these variables are consistent
	
		# Functions to calculate SLC contribution
		
		calculate_contribution <- function(data) {
				contribution <- ((data$perc_UB - data$perc_LB) * (data$FPL/100 - data$fpl_LB)/
								(data$fpl_UB - data$fpl_LB) + data$perc_LB) * 
									(data$poverty_threshold/12 * data$FPL/100)
				return(contribution)
		}

		calculate_contribution_300to400 <- function(data) {
				contribution <- data$perc_LB * (data$poverty_threshold/12 * data$FPL/100)
				return(contribution)
		}
	
		# Add back variables you need to calculate subsidy/contribution (you should not have deleted them earlier!)

			# Income bounds for each income bracket
			
			income_pieces <- c("138% FPL or less","138% FPL to 150% FPL","150% FPL to 200% FPL",
				"200% FPL to 250% FPL","250% FPL to 300% FPL","300% FPL to 400% FPL","400% FPL or greater")
			income_breakpoints <- c(0,1.38,1.5,2,2.5,3,4)
			
			names(income_breakpoints) <- income_pieces[1:length(income_pieces)]
			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece))),"fpl_LB"] <- 
				income_breakpoints[households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece))),"subsidy_linear_piece"]]
		
			names(income_breakpoints) <- c("whatever",income_pieces[1:length(income_pieces)-1])
			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece))),"fpl_UB"] <- 
				income_breakpoints[households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece))),"subsidy_linear_piece"]]
		
			# Percentage contribution bounds for each income bracket
		
			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2014)),
					"perc_LB"] <- 
				contribution_percentages[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2014)),
					"fpl_LB"]),"YR2014"]
		
			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2015)),
					"perc_LB"] <- 
				contribution_percentages[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2015)),
					"fpl_LB"]),"YR2015"]

			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2016)),
					"perc_LB"] <- 
				contribution_percentages[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2016)),
					"fpl_LB"]),"YR2016"]

			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2017)),
					"perc_LB"] <- 
				contribution_percentages[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2017)),
					"fpl_LB"]),"YR2017"]

			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2018)),
					"perc_LB"] <- 
				contribution_percentages[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2018)),
					"fpl_LB"]),"YR2018"]					
			
			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2019)),
					"perc_LB"] <- 
				contribution_percentages[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2019)),
					"fpl_LB"]),"YR2019"]					
						
			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2014)),
					"perc_UB"] <- 
				contribution_percentages[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2014)),
					"fpl_UB"]),"YR2014"]
		
			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2015)),
					"perc_UB"] <- 
				contribution_percentages[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2015)),
					"fpl_UB"]),"YR2015"]
		
			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2016)),
					"perc_UB"] <- 
				contribution_percentages[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2016)),
					"fpl_UB"]),"YR2016"]
			
			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2017)),
					"perc_UB"] <- 
				contribution_percentages[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2017)),
					"fpl_UB"]),"YR2017"]
					
			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2018)),
					"perc_UB"] <- 
				contribution_percentages[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2018)),
					"fpl_UB"]),"YR2018"]
					
			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2019)),
					"perc_UB"] <- 
				contribution_percentages[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2019)),
					"fpl_UB"]),"YR2019"]
			
			# Poverty line
		
			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2014)),
					"poverty_threshold"] <- 
				poverty_guidelines[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2014)),
					"implied_household_size"]),"YR2014"]
			
			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2015)),
					"poverty_threshold"] <- 
				poverty_guidelines[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2015)),
					"implied_household_size"]),"YR2015"]
		
			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2016)),
					"poverty_threshold"] <- 
				poverty_guidelines[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2016)),
					"implied_household_size"]),"YR2016"]

			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2017)),
					"poverty_threshold"] <- 
				poverty_guidelines[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2017)),
					"implied_household_size"]),"YR2017"]

			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2018)),
					"poverty_threshold"] <- 
				poverty_guidelines[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2018)),
					"implied_household_size"]),"YR2018"]	
			
			households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2019)),
					"poverty_threshold"] <- 
				poverty_guidelines[as.character(households[intersect(c(family_indices,single_indices),
					which(!is.na(households$subsidy_linear_piece) & households$year == 2019)),
					"implied_household_size"]),"YR2019"]
			
		# Create rating factor variable
		
		data$rating_factor <- age_rating_factors[as.character(pmin(64,data$age)),"Rating_Factor"]
		data[data$year >= 2018,"rating_factor"] <- 
			age_rating_factors[as.character(pmin(64,data[data$year >= 2018,"age"])),"Rating_Factor2018"]
		
		households$rating_factor <- by(data$rating_factor,data$household_year,sum)[rownames(households)]
		
	
		# 1) Check singles
		
			# Gross Premium
			households[,"gross_premium_amt_int"] <- 
				households[,"premium21"] * 
				households[,"rating_factor"] 
		
			# SLC contribution
			households[intersect(single_indices,
				which(households$subsidy_linear_piece != "300% FPL to 400% FPL" & 
					!is.na(households$subsidy_linear_piece))),"SLC_contribution"] <- 
			calculate_contribution(households[intersect(single_indices,
				which(households$subsidy_linear_piece != "300% FPL to 400% FPL" & 
					!is.na(households$subsidy_linear_piece))),])
						
			households[intersect(single_indices,
				which(households$subsidy_linear_piece == "300% FPL to 400% FPL" & 
					!is.na(households$subsidy_linear_piece))),"SLC_contribution"] <- 
			calculate_contribution_300to400(households[intersect(single_indices,
				which(households$subsidy_linear_piece == "300% FPL to 400% FPL" & 
					!is.na(households$subsidy_linear_piece))),])
			
			# Subsidy
			households[single_indices,"subsidy"] <- 
				pmax(0,households[single_indices,"premiumSLC"] - 
					households[single_indices,"SLC_contribution"]) 
			
			# Net Premium
				# New Definition: out-of-pocket premium for subsidized members of household (NA for unsubsidized)
				# For those ineligible for subsidies, the NET Premium will be NA
				# For those enrolling in catastrophic, the NET Premium will be NA 
			
				# Subsidized Folks
				households[single_indices,"net_premium_amt_int"] <- 
					pmax(0,households[single_indices,"gross_premium_amt_int"] - 
					households[single_indices,"subsidy"])
			
				# No one on catastrophic should be able to access the subsidy (I don't need to do this here)
				households[intersect(single_indices,
					which(households$metal_level_enhanced == "Minimum Coverage")),"net_premium_amt_int"] <- NA
				
			# Sanity checks:
				# Anyone unsubsidized should have NA subsidy - (OK except flagged households)
				# Anyone with income > 400% of FPL should have NA subsidy - (OK except flagged households)
			
			# Import variables
			to.import <- c("gross_premium_amt_int","net_premium_amt_int",
				"SLC_contribution","subsidy","subsidy_linear_piece","FPL",
				"subsidy_fpl_bracket","implied_household_size","flagged")
			singles$SLC_contribution <- NA
			data$SLC_contribution <- NA
			singles$FPL <- singles$subsidy_fpl_percent_int
			data$FPL <- data$subsidy_fpl_percent_int
			singles$subsidy_fpl_percent_int <- NULL
			data$subsidy_fpl_percent_int <- NULL
			
			singles[,to.import] <- households[single_households,to.import]
			rownames(singles) <- paste(singles$individual_id,singles$year,sep="_")
			data[rownames(singles),c(to.import,"premiumSLC")] <- 
				singles[,c(to.import,"premiumSLC")]
			
			rm(singles)
			gc()
			
		# 2) Check families
	
			# SLC contribution
			households[intersect(family_indices,
				which(households$subsidy_linear_piece != "300% FPL to 400% FPL" & 
					!is.na(households$subsidy_linear_piece))),"SLC_contribution"] <- 
			calculate_contribution(households[intersect(family_indices,
				which(households$subsidy_linear_piece != "300% FPL to 400% FPL" & 
					!is.na(households$subsidy_linear_piece))),])
						
			households[intersect(family_indices,
				which(households$subsidy_linear_piece == "300% FPL to 400% FPL" & 
					!is.na(households$subsidy_linear_piece))),"SLC_contribution"] <- 
			calculate_contribution_300to400(households[intersect(family_indices,
				which(households$subsidy_linear_piece == "300% FPL to 400% FPL" & 
					!is.na(households$subsidy_linear_piece))),])
			
			households[intersect(family_indices,
				which(is.na(households$subsidy_linear_piece))),"SLC_contribution"] <- NA
			
			# Subsidy
				# Applies only to the SLC premium facing the subsidized members of the household
				# Need to take out the SLC premium facing the unsubsidized members of the household
			
				# Update subsidy status in family object
			
				number_subsidized <- function(x) {
					return(length(x[x == "SUBSIDIZED"]))
					}
				
				number_unsubsidized <- function(x) {
					return(length(x[x == "UNSUBSIDIZ"]))
				}
				
				subsidized_members <- by(families$subsidy_eligible,
					families$household_year,number_subsidized)
				unsubsidized_members <- by(families$subsidy_eligible,
					families$household_year,number_unsubsidized)
			
				households[,c("old_subsidized_members","old_unsubsidized_members")] <- NA
				households[names(subsidized_members),"old_subsidized_members"] <- subsidized_members
				households[names(unsubsidized_members),"old_unsubsidized_members"] <- unsubsidized_members
				
				change.in.subsidized <- intersect(family_indices,
					which(households$subsidized_members != households$old_subsidized_members))
				families[families$household_year %in% change.in.subsidized,
					"subsidy_eligible"] <- "Subsidy Eligible"
				
				# Make sure remaining mixed subsidy households are flagged
				mixed_subsidy_households <- 
					which(households$unsubsidized_members > 0 & 
						households$subsidized_members > 0 & !is.na(households$subsidized_members))
				households[mixed_subsidy_households,"flagged"] <- 1
				
			households[family_indices,"subsidy"] <- pmax(0,households[family_indices,"premiumSLC"] - 
				households[family_indices,"SLC_contribution"]) 
		
			# Net Premium
				# New Definition: out-of-pocket premium for subsidized members of household (NA for unsubsidized)
				# For those ineligible for subsidies, we will populate the unsubsidized_premium_portion variable
				# For those enrolling in catastrophic, the NET Premium will be NA and we will populate unsubsidized premium 	
			
				# Subsidized Folks
				households[family_indices,"net_premium_amt_int"] <- 
					pmax(0,households[family_indices,"gross_premium_amt_int"] - 
					households[family_indices,"subsidy"])
			
				# No one on catastrophic should get a subsidy
				
				compute_catastrophic_SLC_portion <- function(household,families) {
					catastrophic_members <- intersect(catastrophic_subsidy_eligibles,
						which(families$household_year == household))
					catastrophic_portion <- sum(families[catastrophic_members,"premiumSLC"])
					return(catastrophic_portion)
				}
				
				catastrophic_subsidy_eligibles <- 
					which(families$household_year %in% family_households & 
					families$metal_level_enhanced == "Minimum Coverage"
					& families$subsidy_eligible == "Subsidy Eligible")
				catastrophic_households <- unique(families[catastrophic_subsidy_eligibles,"household_year"])
				
				premiumSLC_catastrophic <- rep(0,length(catastrophic_households))
				names(premiumSLC_catastrophic) <- catastrophic_households
				for(household in catastrophic_households) {
					catastrophic_members <- intersect(catastrophic_subsidy_eligibles,
						which(families$household_year == household))
					premiumSLC_catastrophic[household] <- 
						sum(families[catastrophic_members,"premiumSLC"])
				}
				
				#premiumSLC_catastrophic <- sapply(catastrophic_households,FUN=compute_catastrophic_SLC_portion,
				#	families[families$household_year %in% catastrophic_households,
				#	c("household_year","premiumSLC")])
					
				households$premiumSLC_catastrophic <- 0
				households[names(premiumSLC_catastrophic),"premiumSLC_catastrophic"] <- 
					premiumSLC_catastrophic
				
				households[catastrophic_households,"net_premium_amt_int"] <- 
				households[catastrophic_households,"net_premium_amt_int"] + 
					pmax(0,households[catastrophic_households,"subsidy"] - 
					households[catastrophic_households,"premiumSLC_catastrophic"])
				
			# Even for flagged records, I want to make sure unsubsidized people don't get subsidies
			households[households$flagged & households$subsidized_members == 0 &
				!is.na(households$subsidized_members),
				c("SLC_contribution","subsidy","net_premium_amt_int")] <- NA
			households[households$flagged & households$FPL > 400 & !is.na(households$FPL > 400),
				c("SLC_contribution","subsidy","net_premium_amt_int")] <- NA
				
			# Import variables
			to.import <- c("gross_premium_amt_int","net_premium_amt_int","SLC_contribution",
				"subsidy","subsidy_linear_piece",
				"FPL","subsidy_fpl_bracket","implied_household_size","flagged")
			families$FPL <- NA
			
			families[,to.import] <- households[families$household_year,to.import]
			rownames(families) <- paste(families$individual_id,families$year,sep="_")
			data[rownames(families),c(to.import,"premiumSLC")] <- 
				families[,c(to.import,"premiumSLC")]
			
			rm(families)
			gc()
			
			# Need to check the acs plan/silver plan consistency in the household
			assign.to.silver <- which(((data$FPL > 250 & !is.na(data$FPL)) | 
				data$subsidy_eligible == "Not Subsidy Elig") & 
				data$metal_level_enhanced %in% 
				c("Silver - Enhanced 73","Silver - Enhanced 87","Silver - Enhanced 94"))
			assign.to.acs73 <- which(data$subsidy_fpl_bracket == "200% FPL to 250% FPL" & 
				data$metal == "Silver" & data$metal_level_enhanced != "Silver - Enhanced 73"  & 
				data$subsidy_eligible == "Subsidy Eligible")
			assign.to.acs87 <- which(data$subsidy_fpl_bracket == "150% FPL to 200% FPL" & 
				data$metal == "Silver" & data$metal_level_enhanced != "Silver - Enhanced 87"  & 
				data$subsidy_eligible == "Subsidy Eligible")
			assign.to.acs94 <- which(data$subsidy_fpl_bracket %in% 
				c("138% FPL to 150% FPL","138% FPL or less") & data$metal == "Silver" & 
				data$metal_level_enhanced != "Silver - Enhanced 94"  & 
				data$subsidy_eligible == "Subsidy Eligible")
			
			
			if(length(assign.to.silver) > 0) data[assign.to.silver,"metal_level_enhanced"] <- "Silver"
			if(length(assign.to.acs73) > 0) data[assign.to.acs73,"metal_level_enhanced"] <- "Silver - Enhanced 73"
			if(length(assign.to.acs87) > 0) data[assign.to.acs87,"metal_level_enhanced"] <- "Silver - Enhanced 87"
			if(length(assign.to.acs94) > 0) data[assign.to.acs94,"metal_level_enhanced"] <- "Silver - Enhanced 94"
		
			plan_ids <- 1:nrow(plan_data)
			names(plan_ids) <- paste(plan_data$HIOS,plan_data$metal_level,plan_data$ENROLLMENT_YEAR,plan_data$region,sep="_")
			
			data$plan_id <- paste(data$hios_id_14,data$metal_level_enhanced,data$year,data$region,sep="_")
			data[data$year %in% c(2014,2015) & data$insurer != "SHARP","plan_id"] <- 
			paste(substr(data[data$year %in% c(2014,2015) & data$insurer != "SHARP","hios_id_14"],1,10),
				data[data$year %in% c(2014,2015) & data$insurer != "SHARP","metal_level_enhanced"],
				data[data$year %in% c(2014,2015) & data$insurer != "SHARP","year"],
				data[data$year %in% c(2014,2015) & data$insurer != "SHARP","region"],sep="_")
			
			data$plan_id <- plan_ids[data$plan_id]
			data$premium21 <- plan_data[data$plan_id,"Premium"]/1.278
			data$plan_name <- as.character(plan_data[data$plan_id,"Plan_Name2"])
				
			
			# Recompute plans per household
			compute.number.unique <- function(x) {
				return(length(unique(x)))
			}
			plans_per_household <- by(data[data$household_year %in% family_households,"plan_id"],
				data[data$household_year %in% family_households,"household_year"],compute.number.unique)
			households[family_households,"plans"] <- plans_per_household[family_households]
			
	# Delete columns we don't need from household object
	
		# Household Object
		to.delete <- c("number_gross_premiums","number_net_premiums","income_brackets","number_age_unknown","target_factor",
						"number_65plus","number_55to64","number_45to54","number_35to44","number_26to34","number_18to25","number_0to17",
						"ALT_GROSS_PREMIUM_AMT","ALT_NET_PREMIUM_AMT","alt_subsidy","alt_SLC_contribution","alt_target_factor","number_income_unknown",
						"number_gt400","number_250to400","number_200to250","number_150to200","number_138to150","number_lt138",
						"number_silver73","number_silver87","number_silver94","alt_subsidy_fpl_bracket","implied_subsidy_bracket",
						"income.check","zero_subsidy","unsubsidized_premium_portion","old_subsidized_members","old_unsubsidized_members",
						"rating_factor","ages_of_members","fpl_lower_bound","fpl_upper_bound","zero_premium")

		households <- households[,!colnames(households) %in% to.delete]
		households <- households[,sort(colnames(households))]

		to.delete <- c("ahbx_case_id_x","plan_type","plan_mrkt","placed_in_household","case_year",
			"split_flag","orig_members")
		
	# Save the data objects 
		save(data,file="data/final/enroll_temp")
		save(households,file="data/final/household_temp")
		gc()			
					
		