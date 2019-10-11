
###########################################################################
## Project:       Decision Assistance and Insurance Choice
## Author:        E. Saltzman & I. McCarthy
## Date Created:  10/11/2019
## Date Edited:   10/11/2019
###########################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)


# Preliminaries -----------------------------------------------------------

## Load data
setwd("D:/CloudStation/Professional/Research Projects/ACA Decision Support/data")
data <- get(load("ca_enrollment_data_AUG022019")) # individual-level data
households <- get(load("ca_household_data_AUG022019")) # household-level data
plan_data <- read.csv("ca_plan_data2.csv") # Covered California plan data
zip3_choices <- read.csv("zip3_choices2.csv",row.names = 1) # choice set by 3 digit zip and rating area
product_definitions <- read.csv("product_definitions.csv",row.names = 1) # definitions of column names in zip3_choices



# Clean Plan Data ---------------------------------------------------------

## rename insurers
plan_data <- plan_data %>%
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
plan_data <- plan_data %>%
  mutate(metal = as.character(metal_level),
         metal = replace(metal, metal %in% c("Silver - Enhanced 73",
                                             "Silver - Enhanced 87",
                                             "Silver - Enhanced 94"), 
                         "Silver") )



# Clean Individual Data ---------------------------------------------------

## Drop all uninsured records for this analysis
data <- data %>%
  filter(!is.na(plan_id))

## Create Age Group Variable
data <- data %>%
  mutate(
    age_group=case_when(
      AGE < 18 ~ "0to17",
      AGE >= 18 & AGE < 26 ~ "18to25",
      AGE >= 26 & AGE < 35 ~ "26to34",
      AGE >= 35 & AGE < 45 ~ "35to44",
      AGE >= 45 & AGE < 55 ~ "45to54",
      AGE >= 55 & AGE < 65 ~ "55to64",
      AGE >= 65 & AGE < 120 ~ "65plus",
      TRUE ~ NA_character_
    )
  )


## Genders
data <- data %>%
  mutate(
    Gender = case_when(
      gender == 1 ~ "Male",
      gender == 0 ~ "Female"
    )
  )

# Metal
data <- data %>%
  mutate(metal = as.character(metal_level_enhanced),
         metal = replace(metal, metal %in% c("Silver - Enhanced 73",
                                             "Silver - Enhanced 87",
                                             "Silver - Enhanced 94"), 
                         "Silver"),
         metal = replace(metal, metal == "Minimum Coverage", "Catastrophic"))


# Subsidized/Unsubsidized








data$subsidy_eligible <- as.numeric(households[data$household_year,"subsidized_members"] > 0)
data$subsidized <- as.character(data$subsidy_eligible) 
data[data$subsidized == "1" & !is.na(data$subsidized),"subsidized"] <- "Subsidized"
data[data$subsidized == "0" & !is.na(data$subsidized),"subsidized"] <- "Unsubsidized"

# CSR Eligible
data$csr_eligible <- as.numeric(data$subsidized == "Subsidized" & data$FPL <= 2.50)


# Plan network type/etc
plan_data$PLAN_NETWORK_TYPE <- as.character(plan_data$PLAN_NETWORK_TYPE)
data$plan_network_type <- NA
data[!is.na(data$plan_id),"plan_network_type"] <- plan_data[data[!is.na(data$plan_id),"plan_id"],"PLAN_NETWORK_TYPE"]

# Income Groups
data$subsidy_fpl_bracket <- NA
data[data$FPL <= 1.38 & !is.na(data$FPL),"subsidy_fpl_bracket"] <- "138% FPL or less"
data[data$FPL > 1.38 & data$FPL <= 1.50 & !is.na(data$FPL),"subsidy_fpl_bracket"] <- "138% FPL to 150% FPL"
data[data$FPL > 1.50 & data$FPL <= 2.00 & !is.na(data$FPL),"subsidy_fpl_bracket"] <- "150% FPL to 200% FPL"
data[data$FPL > 2.00 & data$FPL <= 2.50 & !is.na(data$FPL),"subsidy_fpl_bracket"] <- "200% FPL to 250% FPL"
data[data$FPL > 2.50 & data$FPL <= 4.00 & !is.na(data$FPL),"subsidy_fpl_bracket"] <- "250% FPL to 400% FPL"
data[data$FPL > 4.00 & !is.na(data$FPL),"subsidy_fpl_bracket"] <- "400% FPL or greater"

# Language
data$language <- as.character(data$language_spoken)
data[data$language == "English","language"] <- "English"
data[data$language == "Spanish","language"] <- "Spanish"
data[!data$language %in% c("(nonres","English","Spanish"),"language"] <- "Other Language"
data[data$language == "(nonres","language"] <- NA

# Previous plan
data$previous_plan_number <- households[data$household_year,"previous_plan_number"]

# Dominated Plan
data$dominated_choice <- NA

data[data$csr_eligible == 1 & !is.na(data$csr_eligible == 1) & 
       data$subsidy_fpl_bracket %in% c("138% FPL or less","138% FPL to 150% FPL") &
       data$metal  %in% c("Gold","Platinum"),"dominated_choice"] <- 1
data[data$csr_eligible == 1 & !is.na(data$csr_eligible == 1) & 
       data$subsidy_fpl_bracket %in% c("138% FPL or less","138% FPL to 150% FPL") &
       !data$metal  %in% c("Gold","Platinum"),"dominated_choice"] <- 0

data[data$csr_eligible == 1 & !is.na(data$csr_eligible == 1) & 
       data$subsidy_fpl_bracket == "150% FPL to 200% FPL" &
       data$metal  %in% c("Gold"),"dominated_choice"] <- 1
data[data$csr_eligible == 1 & !is.na(data$csr_eligible == 1) & 
       data$subsidy_fpl_bracket == "150% FPL to 200% FPL" &
       !data$metal  %in% c("Gold"),"dominated_choice"] <- 0

# CSR choose bronze_flag
data$csr_chose_bronze <- NA

data[data$csr_eligible == 1 & !is.na(data$csr_eligible == 1) & 
       data$subsidy_fpl_bracket %in% c("138% FPL or less","138% FPL to 150% FPL","150% FPL to 200% FPL") &
       data$metal  %in% c("Bronze","Catastrophic"),"csr_chose_bronze"] <- 1
data[data$csr_eligible == 1 & !is.na(data$csr_eligible == 1) & 
       data$subsidy_fpl_bracket %in% c("138% FPL or less","138% FPL to 150% FPL","150% FPL to 200% FPL") &
       !data$metal  %in% c("Bronze","Catastrophic"),"csr_chose_bronze"] <- 0



# Summary Statistics ------------------------------------------------------



#### Compare Demographics of OEP vs. SEP

	metals <- c("Catastrophic","Bronze","Silver","Gold","Platinum")
	network_types <- c("HMO","PPO","EPO","HSP")
	msps <- c("non-MSP","MSP")
	hsas <- c("non-HSA","HSA")
	plan_parameters <- c("Average standardized premium","Average deductible","Average max. out-of-pocket","Average coinsurance")
	insurers <- c("Anthem","Blue_Shield","Chinese_Community","Contra_Costa",
		"Health_Net","Kaiser","LA_Care","Molina","Oscar","SHARP","United","Valley","Western")
	breadth_cats <- c("< 10%","10% to 20%","20% to 30%","> 30%")
		
	age_groups <- c("0to17","18to25","26to34","35to44","45to54","55to64","65plus")
	income_groups <- c("138% FPL or less","138% FPL to 150% FPL","150% FPL to 200% FPL",
		"200% FPL to 250% FPL","250% FPL to 400% FPL","400% FPL or greater")
	genders <- c("Female","Male")
	smoker_groups <- c("Non-Smoker","Smoker")
	race_groups <- c("Asian","Black/African American",
		"Hispanic","Other Race","White")
	languages <- c("English","Spanish","Other Language")
	years <- c("2014","2015","2016","2017","2018","2019")
	#subsidy_groups <- c("Eligible for PTCs","Eligible for CSRs","Exempt from Mandate","Subject to Mandate")
	subsidy_groups <- c("Subsidized","Unsubsidized")
	
	product_fields <- c(metals,network_types)
	fields <- c("Total population",product_fields,years,income_groups,subsidy_groups,age_groups,genders,race_groups,languages)

	# Create/modify variables
	
	
	
	
	compute_population_choices <- function(data,fields) {
		
		pop_output <- rep(NA,length(fields))
		names(pop_output) <- fields
	
		# Total Population
		pop_output["Total population"] <- nrow(data)

		#data <- data[!data$flagged,]
		
		# Metals
		if(any(fields %in% metals)) {
			pop_output[metals] <- by(data$metal,data$metal,length)[metals]/
				(nrow(data) - length(which(is.na(data$metal))))
		}
		
		# Plan Parameters
		if(any(plan_parameters %in% fields)) {
			pop_output["Average standardized premium"] <- weighted.mean(plan_data[data$plan_unique_id,"Premium"],data$PERWT,na.rm=T)
			pop_output["Average deductible"] <- weighted.mean(plan_data[data$plan_unique_id,"Deductible"],data$PERWT,na.rm=T)
			pop_output["Average max. out-of-pocket"] <- weighted.mean(plan_data[data$plan_unique_id,"OOPMAX"],data$PERWT,na.rm=T)
			pop_output["Average coinsurance"] <- weighted.mean(plan_data[data$plan_unique_id,"ER_coins"],data$PERWT,na.rm=T)
		}
	
		# Network Types
		if(any(network_types %in% fields)) {
			pop_output[network_types] <- by(data$plan_network_type,data$plan_network_type,length)[network_types]/
				(nrow(data) - length(which(is.na(data$plan_network_type))))
		}
	
		# Insurers
		if(any(insurers %in% fields)) {
			pop_output[insurers] <- by(data$insurer,data$insurer,length)[insurers]/
				(nrow(data) - length(which(is.na(data$insurer))))
		}
	
		# Years
		if(any(years %in% fields)) {
			pop_output[years] <- by(data$year,data$year,length)[years]/
				(nrow(data) - length(which(is.na(data$year))))
		}
	
		# Income Groups
		if(any(income_groups %in% fields)) {
			pop_output[income_groups] <- 
				by(data$subsidy_fpl_bracket,data$subsidy_fpl_bracket,length)[income_groups]/
				(nrow(data) - length(which(is.na(data$subsidy_fpl_bracket))))
		}
	
		# Age Groups
		if(any(age_groups %in% fields)) {
			pop_output[age_groups] <- by(data$age_group,data$age_group,length)[age_groups]/
				(nrow(data) - length(which(is.na(data$age_group))))
		}
	
		# Genders
		if(any(genders %in% fields)) {
			pop_output[genders] <- by(data$gender,data$gender,length)/
				(nrow(data) - length(which(is.na(data$gender))))
		}
	
		# Race Groups
		if(any(race_groups %in% fields)) {
			pop_output[race_groups] <- by(data$race,data$race,length)[race_groups]/nrow(data) 
			pop_output[race_groups] <- pop_output[race_groups]/sum(pop_output[race_groups])
			#pop_output["Unreported"] <- 1 - sum(pop_output[race_groups],na.rm=TRUE)
		}
		
		# Subsidy Groups
		if(any(subsidy_groups %in% fields)) {
			pop_output[subsidy_groups] <- by(data$subsidized,data$subsidized,length)[subsidy_groups]/
				(nrow(data) - length(which(is.na(data$subsidized))))
		}
		
		# Languages
		if(any(languages %in% fields)) {
			pop_output[languages] <- by(data$language,data$language,length)[languages]/
				(nrow(data) - length(which(is.na(data$language))))
		}
		
		# Dominated Choice
		if("dominated choice" %in% fields) {
			pop_output["dominated choice"] <- 
				length(which(data$csr_eligible == 1 & !is.na(data$csr_eligible) & 
					data$dominated_choice == 1 & !is.na(data$dominated_choice)))/
				length(which(data$csr_eligible == 1 & !is.na(data$csr_eligible) & data$FPL <= 2.00))
		}
		
		# CSR chose bronze
		if("csr_chose_bronze" %in% fields) {
			pop_output["csr_chose_bronze"] <- 
				length(which(data$csr_eligible == 1 & !is.na(data$csr_eligible) & 
					data$csr_chose_bronze == 1 & !is.na(data$csr_chose_bronze)))/
				length(which(data$csr_eligible == 1 & !is.na(data$csr_eligible) & data$FPL <= 2.00))
		}
		
		return(pop_output)
	}

	# Compare service channels (https://apply.coveredca.com/hix/broker/search?anonymousFlag=Y&recordType=null&recordId=null&lang=en)
		# Certified Insurance Agent (CIA)
		# Plan Based Enroller (PBE)
		# Certified Enrollment Counselor (CEC)
		# Service Center Representative (SCR)
		# County Eligibility Worker(CEW)
		# Unassisted (Unassisted)
	
		# Let's try creating 3 groups
			# Insurance agent
			# Other assistance
			# Unassisted
	
#### NOTE: you should repeat everything here with new enrollees only	
	
	# Everyone
	
		data$channel <- as.character(data$service_channel)
		data[data$channel %in% c("CIA","PBE"),"channel"] <- "Insurance Agent"
		data[data$channel %in% c("SCR","CEW","CEC"),"channel"] <- "Other Assistance"
	
		fields <- c("Total population",product_fields,insurers,years,income_groups,subsidy_groups,age_groups,genders,race_groups,languages,
			"dominated choice","csr_chose_bronze")
		categories <- c("Insurance Agent","Other Assistance","Unassisted")
		
		output <- matrix(NA,length(fields),length(categories),dimnames=list(fields,categories))
		output[,"Insurance Agent"] <- compute_population_choices(data = data[data$channel == "Insurance Agent",],fields = fields)
		output[,"Other Assistance"] <- compute_population_choices(data = data[data$channel == "Other Assistance",],fields = fields)
		output[,"Unassisted"] <- compute_population_choices(data = data[data$channel == "Unassisted",],fields = fields)
		write.csv(output,"descriptive_stats.csv")
	
	# New enrollees only
		
		output <- matrix(NA,length(fields),length(categories),dimnames=list(fields,categories))
		output[,"Insurance Agent"] <- compute_population_choices(data = data[data$channel == "Insurance Agent" & is.na(data$previous_plan_number),],fields = fields)
		output[,"Other Assistance"] <- compute_population_choices(data = data[data$channel == "Other Assistance" & is.na(data$previous_plan_number),],fields = fields)
		output[,"Unassisted"] <- compute_population_choices(data = data[data$channel == "Unassisted" & is.na(data$previous_plan_number),],fields = fields)
		write.csv(output,"descriptive_stats.csv")
	