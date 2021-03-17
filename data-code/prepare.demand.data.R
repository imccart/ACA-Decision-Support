################################
### File to Process ACS Data ###
################################

##### ACS Variables:
	# SERIALNO - Household ID
	# SPORDER - Person Number
	# PUMA - PUMA
	# CIT - citizenship status
	# CITWP - year of naturalization
	# YOEP - year of entry
	# NATIVITY - nativity
	# COW - class of worker
	# HINS1 - ESI/group
	# HINS2 - individual/nongroup
	# HINS3 - Medicare
	# HINS4 - Medicaid
	# HINS5 - TRICARE/military
	# HINS6 - VA
	# HINS7 - Indian Health Service
	# HICOV - Health insurance coverage recode
	# PRIVCOV - Private health insurance coverage recode
	# PUBCOV - Public health insurance coverage recode
	# AGEP - age
	# PWGTP - person weight
	# SEX - gender
	# HISP - Hispanic
	# PINCP - total personal income
	# POVPIP - FPL
	# RAC1P - race
	# RACAIAN - American Indian/Alaska Native
	
	# NPF - number of persons in family
	# RELP - relationship to household head
	
##### Variable we need to merge with exchange data
	# Individual object:
		# individual_id
		# household_id
		# RES_COUNTY
		# region
		# SUBSIDIZED_UNSUBSIDIZED
		# penalty.exempt
		# FPL 
		# subsidy_fpl_bracket
		# subsidy_linear_piece
		# implied_household_size
		# age
		# AGE_BRACKET
		# ENROLLMENT_YEAR
		# GENDER
		# household_year
		# premiumSLC
		# subsidy
		# SLC_contribution
		# Missing values:
			# AHBX_CASE_ID_X
			# HIOS_ID_10
			# GROSS_PREMIUM_AMT (should be zero)
			# NET_PREMIUM_AMT (should be zero)
			# PLAN_LEVEL (check that this is consistent with metal_level_enhanced)
			# metal_level_enhanced
			# ISSUER_NAME
			# PLAN_NETWORK_TYPE
			# plan_id
			# premium21
			# case_year
	# Household object:
		# ages_of_members
		# county
		# region
		# ENROLLMENT_YEAR
		# FPL 
		# fpl_LB
		# fpl_UB
		# perc_LB
		# perc_UB
		# poverty_threshold
		# subsidy_fpl_bracket
		# subsidy_linear_piece
		# members
		# subsidized_members
		# unsubsidized_members
		# implied_household_size
		# plans (should be zero)
		# premiumSLC
		# premiumSLC_catastrophic (should be 0)
		# premiumSLC_unsubsidized (should be 0)
		# SLC_contribution
		# subsidy
		# Missing values:
			# metal_level_enhanced
			# GROSS_PREMIUM_AMT
			# NET_PREMIUM_AMT
			# compositions (could populate this?)
			# premium21
	# New Variables:
		# Weight
		# penalty.exempt
		# penalty

	
input <- parse.SAScii("data/ACS-SIPP/input20143.sas")
rownames(input) <- input$varname
ipums_data <- read.table(gzfile("data/ACS-SIPP/usa_00011.dat.gz"),skipNul = TRUE)

acs <- data.frame(matrix(NA,nrow=dim(ipums_data)[1],ncol=dim(input)[1]))
colnames(acs ) <- rownames(input)

counter <- 0
for(i in colnames(acs )) { 
	acs [,i] <- as.numeric(substr(as.character(ipums_data[,1]),start=counter+1,stop=counter+input[i,"width"]))
	counter <- counter+input[i,"width"]
}
rm(ipums_data)
gc()
	
##### Load All Other Data 
acs_immigration <- get(load("data/final/acs_immigration"))
acs_emp_offer <- get(load("data/final/acs_emp_offer"))
ca_pumas <- read.csv("data/PUMAs.csv",header=TRUE) # PUMA definitions for California
sahie <- read.csv("data/sahie_2014.csv",header=TRUE) # 2014 CA SAHIE data
contribution_percentages <- read.csv("data/contribution_percentages.csv",row.names = 1) # ACA contribution percentages
poverty_guidelines <- read.csv("data/poverty_guidelines.csv",row.names=1) # Poverty guidelines
plan_data <- read.csv("data/plan_data.csv") # Covered California plan data
rating_areas <- read.csv("data/rating_areas.csv",row.names = 1) # California county-rating area mapping
zip3_choices <- read.csv("data/zip3_choices.csv",row.names = 1) # choice set by 3 digit zip and rating area
product_definitions <- read.csv("data/product_definitions.csv",row.names = 1) # definitions of column names in zip3_choices
counties2014 <- read.csv("data/Counties/counties_2014.csv",row.names = 1) # Insurer entry and county-rating area mapping for 2014
counties2015 <- read.csv("data/Counties/counties_2015.csv",row.names = 1) # Insurer entry and county-rating area mapping for 2015
counties2016 <- read.csv("data/Counties/counties_2016.csv",row.names = 1) # Insurer entry and county-rating area mapping for 2016
counties2017 <- read.csv("data/Counties/counties_2017.csv",row.names = 1) # Insurer entry and county-rating area mapping for 2017
counties2018 <- read.csv("data/Counties/counties_2018.csv",row.names = 1) # Insurer entry and county-rating area mapping for 2018
counties2019 <- read.csv("data/Counties/counties_2019.csv",row.names = 1) # Insurer entry and county-rating area mapping for 2018
age_rating_factors <- read.csv("data/age_rating_factors.csv",row.names=1) # CCIIO default rating curve
set.seed(1)

# NOTE: I am reordering plan data because I defined a plan_id number earlier based on a previous ordering
#plan_data$Plan_ID_Order <- as.character(plan_data$Plan_ID_Order)
#rownames(plan_data) <- paste(plan_data$Plan_Name,plan_data$ENROLLMENT_YEAR,
#	plan_data$region,sep="")
#plan_data <- plan_data[plan_data$Plan_ID_Order,]


##### Merge in Immigration Data - imputed from 2008 SIPP Topical Module 2 in process.SIPP.data.R
undocumented_immigrants <- rownames(acs_immigration[acs_immigration$undocumented_immigrant == 1,])
permanent_residents <- rownames(acs_immigration[acs_immigration$permanent_resident == 1,])
temporary_residents <- rownames(acs_immigration[acs_immigration$temporary_resident == 1,])
rm(acs_immigration)

acs$immigration_id <- paste(acs$HIUID,acs$PERNUM,acs$AGE,acs$YEAR,sep="_")

acs$undocumented_immigrant <- 0
acs$permanent_resident <- 0
acs$temporary_resident <- 0

acs[acs$immigration_id %in% undocumented_immigrants,"undocumented_immigrant"] <- 1
acs[acs$immigration_id %in% permanent_residents,"permanent_resident"] <- 1
acs[acs$immigration_id %in% temporary_residents,"temporary_resident"] <- 1

	
##### Merge in Employer Offer Data - imputed from 2008 SIPP Topical Module 6 in process.SIPP.data.R	
imputed_offers <- rownames(acs_emp_offer[acs_emp_offer$employer_offer == 1,])	
acs$imputed_offer <- 0
acs[acs$immigration_id %in% imputed_offers,"imputed_offer"] <-  1
rm(acs_emp_offer)

	# Define household id
	acs$household_id <- paste(acs$HIUID,acs$FAMUNIT,sep="_")
	acs$household_year <- paste(acs$household_id,acs$YEAR,sep="_")
	
	# Imputed Offers
	imputed_offers <- by(acs$imputed_offer,acs$household_year,sum)
	
	# Number in household already on ESI 
	acs$ESI <- as.numeric(acs$HINSEMP == 2)
	number_ESI <- by(acs$ESI,acs$household_year,sum)
	
	# Access to employer offer
	access_to_emp_offer <- as.numeric(imputed_offers + number_ESI > 0)
	names(access_to_emp_offer) <- names(number_ESI)
	
	acs$access_to_emp_offer <- 0
	acs[acs$household_year %in% names(access_to_emp_offer[access_to_emp_offer == 1]),"access_to_emp_offer"] <- 1
	
##### Add 2017 and 2018 Records
	# As of 7/29/2019, we don't yet have ACS data for 2018 and 2019
	# The uninsured rate has been pretty flat (between 2016 and 2017, the uninsured rate went from 7.3% to 7.2% (https://californiahealthline.org/news/uninsured-rate-declines-in-california-remains-unchanged-nationally/)
	# For now, I am just going to duplicate the 2017 records for 2018 and 2019 because the uninsured rate appears to be flat
	# This should be adjusted as more data become available
	
years_to_add <- c(2018,2019)

for(t in years_to_add) {

	acs_to_add <- acs[acs$YEAR == 2017,]
	acs_to_add$YEAR <- t
	acs_to_add$household_year <- paste(acs_to_add$household_id,acs_to_add$YEAR,sep="_")
	acs <- rbind(acs,acs_to_add)

}

rm(acs_to_add)
gc()
	
	
##### Population Weights

acs$PERWT <- acs$PERWT/100 
POP_SIZE <- sum(acs$PERWT)

##### Create Household Object

	compute.number.unique <- function(x) {
		return(length(unique(x)))
	}

	
	# Household size and weight
	households <- data.frame(cbind(by(acs$household_year,acs$household_year,length),by(acs$PERWT,acs$household_year,sum)))
	colnames(households) <- c("household_size","weight")
	acs$household_size <- households[acs$household_year,"household_size"]
	
	# Uninsured members and weight
	uninsured <- rownames(acs[acs$HCOVANY == 1 & acs$HCOVPRIV == 1 & acs$HINSEMP == 1 & acs$HINSPUR == 1 & acs$HINSTRI == 1 & 
		acs$HCOVPUB == 1 & acs$HINSCARE == 1 & acs$HINSVA == 1 & acs$HINSIHS == 1,])	
	uninsured_members <- by(acs[uninsured,"household_year"],acs[uninsured,"household_year"],length)
	uninsured_weight <- by(acs[uninsured,"PERWT"],acs[uninsured,"household_year"],sum)
		
	households[,c("enrollees","uninsured_weight")] <- 0
	households[names(uninsured_members),"enrollees"] <- uninsured_members
	households[names(uninsured_weight),"uninsured_weight"] <- uninsured_weight
	
	# Income
	acs$poverty_threshold <- NA
	acs[acs$YEAR == 2014,"poverty_threshold"] <- poverty_guidelines[as.character(acs[acs$YEAR == 2014,"household_size"]),"YR2014"]
	acs[acs$YEAR == 2015,"poverty_threshold"] <- poverty_guidelines[as.character(acs[acs$YEAR == 2015,"household_size"]),"YR2015"]
	acs[acs$YEAR == 2016,"poverty_threshold"] <- poverty_guidelines[as.character(acs[acs$YEAR == 2016,"household_size"]),"YR2016"]
	acs[acs$YEAR == 2017,"poverty_threshold"] <- poverty_guidelines[as.character(acs[acs$YEAR == 2017,"household_size"]),"YR2017"]
	acs[acs$YEAR == 2018,"poverty_threshold"] <- poverty_guidelines[as.character(acs[acs$YEAR == 2018,"household_size"]),"YR2018"]
	acs[acs$YEAR == 2019,"poverty_threshold"] <- poverty_guidelines[as.character(acs[acs$YEAR == 2019,"household_size"]),"YR2018"]
	
	
	acs[acs$POVERTY > 500,"POVERTY"] <- pmax(501,(acs$FTOTINC/acs$poverty_threshold)[acs$POVERTY > 500]*100)
	acs$FPL <- acs$POVERTY/100
	households$FPL <-  by(acs$FPL,acs$household_year,max)[rownames(households)]
	acs$POVERTY <- NULL
		
		#zz <- by(acs$FPL,acs$household_year,max)[rownames(households)]
		#z <- by(acs$FPL,acs$household_year,compute.number.unique)[rownames(households)]
	
	# Subsidy linear piece and subsidy bracket
	income_pieces <- c("138% FPL or less","138% FPL to 150% FPL","150% FPL to 200% FPL",
			"200% FPL to 250% FPL","250% FPL to 300% FPL","300% FPL to 400% FPL",NA)
	income_breakpoints <- c(0,1.3799999,1.5,2,2.5,3,4.0000001,10000)
	names(income_breakpoints) <- income_pieces[1:length(income_pieces)]	
	households$subsidy_linear_piece <- income_pieces[findInterval(households$FPL,income_breakpoints)]
	
	# Geography 
	rownames(ca_pumas) <- ca_pumas$PUMA5CE
	households$PUMA <-  by(acs$PUMA,acs$household_year,unique)[rownames(households)]
	households$rating_area <- ca_pumas[as.character(households$PUMA),"RATING_AREA"]
	households$county <- ca_pumas[as.character(households$PUMA),"COUNTY"]
	
		# Some PUMAs map to multiple counties
		households$problem_county <- by(acs$COUNTYFIP,acs$household_year,sum)[rownames(households)]
		households$problem_county <- households$problem_county == 0
		problem_PUMAs <- table(households[households$problem_county,"PUMA"])
		
		# SAHIE - contains aggregate uninsured distribution in each county
		sahie <- sahie[sahie$statefips == 6 & sahie$geocat == 50 & sahie$agecat == 0 & sahie$racecat == 0 & sahie$sexcat == 0 & sahie$iprcat == 0,]
		rownames(sahie) <- sahie$countyfips
	
		
		# I think we can assign PUMAs 5301 and 5302 to Monterey County (but not 5303) - no choice set difference
		households[households$PUMA %in% c(5301,5302),"county"] <- "Monterey"	
		households[households$PUMA %in% c(5301,5302),"problem_county"] <- FALSE
		
		# For the other counties, we will assign randomly to match distribution in SAHIE
		
		households$county <- as.character(households$county)
		
		assign_counties <- function(households,PUMA,puma_counties) {
		
			sahie_puma <- sahie[sahie$countyfips %in% names(puma_counties),]
			uninsured_distribution <- cumsum(as.numeric(as.vector(sahie_puma$NUI)))/sum(as.numeric(as.vector(sahie_puma$NUI)))
			names(uninsured_distribution) <- puma_counties[rownames(sahie_puma)]

			household_ordering <- sample(rownames(households[households$PUMA == PUMA,]), replace=FALSE)
			household_weights <- cumsum(households[household_ordering,"uninsured_weight"])/sum(households[household_ordering,"uninsured_weight"])
			names(household_weights) <- household_ordering
			
			for(county in names(uninsured_distribution)) {
				if(uninsured_distribution[county] == 1) { # reached last county
					households[household_ordering,"county"] <- county
				} else {
					target <- uninsured_distribution[county]
					final.element.assigned <- which.max(household_weights[household_weights - target < 0])
					assigned_households <- household_ordering[1:final.element.assigned]
					households[assigned_households,"county"] <- county
					
					household_weights <- household_weights[(final.element.assigned + 1):length(household_ordering)]
					household_ordering <- household_ordering[(final.element.assigned + 1):length(household_ordering)]
				}
			}
			
			return(households)
		}
		
		
		# PUMA 300: Alpine,Amador,Calaveras,Inyo,Mariposa,Mono,Tuolumne
			# Inyo and Mono are in rating area 3
			# All other counties are in rating area 1
		
		puma_counties <- c("Alpine","Amador","Calaveras","Inyo","Mariposa","Mono","Tuolumne")			
		names(puma_counties) <- c(3,5,9,27,43,51,109)
		households <- assign_counties(households,300,puma_counties)
		
		households[households$county %in% c("Alpine","Amador","Calaveras","Mariposa","Tuolumne"),"rating_area"] <- 1
		households[households$county %in% c("Inyo","Mono"),"rating_area"] <- 13
		
		# PUMA 1100: Colusa, Glenn, Tehama & Trinity Counties - no choice set difference
			# All counties in rating area 1
			
		puma_counties <- c("Colusa","Glenn","Tehama","Trinity")			
		names(puma_counties) <- c(11,21,103,105)
		households <- assign_counties(households,1100,puma_counties)
		
		# PUMA 1500: Del Norte,Lassen,Modoc,Plumas,Siskiyou - no choice set difference
			# All counties in rating area 1
		
		puma_counties <- c("Del Norte","Lassen","Modoc","Plumas","Siskiyou")			
		names(puma_counties) <- c(15,35,49,63,93)
		households <- assign_counties(households,1500,puma_counties)
		
		# PUMA 3300: Lake,Mendocino - no choice set difference
			# All counties in rating area 1
		
		puma_counties <- c("Lake","Mendocino")			
		names(puma_counties) <- c(33,45)
		households <- assign_counties(households,3300,puma_counties)
		
		# PUMA 5700: Nevada, Sierra - no choice set difference
			# All counties in rating area 1
		
		puma_counties <- c("Nevada","Sierra")			
		names(puma_counties) <- c(57,91)
		households <- assign_counties(households,5700,puma_counties)
		
		# PUMA 10100: Sutter, Yuba - no choice set difference
			# All counties in rating area 1
		
		puma_counties <- c("Sutter","Yuba")			
		names(puma_counties) <- c(101,115)
		households <- assign_counties(households,10100,puma_counties)
		
		# PUMA 5303: Monterey,San Benito - no choice set difference in 2015
			# All counties in rating area 9
			# Monterey county is also in two other PUMAs (5301 and 5302)
			# For some reason the number of uninsured in PUMAs 5301 and 5302 exceeds the number of uninsured in Monterey according to SAHIE
			# So I am going to assign all of the uninsured to San Bentio in PUMA 5303
			
		households[households$PUMA == "5303","county"] <- "San Benito"
		
		# Change county variable to all caps
		households$county <- toupper(households$county)
		
		# Convert rating area to numeric
		temp <- as.numeric(households$rating_area)
		temp2 <- rep(NA,length(temp))
		temp2[temp == 1] <- 1
		temp2[temp >= 3 & temp <= 12] <- temp[temp >= 3 & temp <= 12] + 7
		temp2[temp >= 13] <- temp[temp >= 13] - 11
		households$rating_area <- temp2
		rm(temp,temp2)
		
		# I need to assign rating areas/refined counties to LA residents
		households[households$rating_area == 15,"county"] <- "LOS ANGELES1"
		households[households$rating_area == 16,"county"] <- "LOS ANGELES2"
		
		# Read in to ACS object
		acs$county <- households[acs$household_year,"county"]
		acs$rating_area <- households[acs$household_year,"rating_area"]

##### Demographic variables:

	# Gender
	acs$gender <- acs$SEX
	acs$SEX <- NULL
	acs[acs$gender == 2,"gender"] <- 0
	households$perc_male <- by(acs$gender,acs$household_year,mean)
	
	# Age Groups
	acs$age_group <- NA
	acs[acs$AGE < 18,"age_group"] <- "0to17"
	acs[acs$AGE >= 18 & acs$AGE < 26,"age_group"] <- "18to25"
	acs[acs$AGE >= 26 & acs$AGE < 35,"age_group"] <- "26to34"
	acs[acs$AGE >= 35 & acs$AGE < 45,"age_group"] <- "35to44"
	acs[acs$AGE >= 45 & acs$AGE < 55,"age_group"] <- "45to54"
	acs[acs$AGE >= 55 & acs$AGE < 65,"age_group"] <- "55to64"
	acs[acs$AGE >= 65,"age_group"] <- "65plus"
	
	number_0to17 <- by(acs[acs$age_group == "0to17","age_group"],acs[acs$age_group == "0to17","household_year"],length)
	number_18to25 <- by(acs[acs$age_group == "18to25","age_group"],acs[acs$age_group == "18to25","household_year"],length)
	number_26to34 <- by(acs[acs$age_group == "26to34","age_group"],acs[acs$age_group == "26to34","household_year"],length)
	number_35to44 <- by(acs[acs$age_group == "35to44","age_group"],acs[acs$age_group == "35to44","household_year"],length)
	number_45to54 <- by(acs[acs$age_group == "45to54","age_group"],acs[acs$age_group == "45to54","household_year"],length)
	number_55to64 <- by(acs[acs$age_group == "55to64","age_group"],acs[acs$age_group == "55to64","household_year"],length)
	number_65plus <- by(acs[acs$age_group == "65plus","age_group"],acs[acs$age_group == "65plus","household_year"],length)
	
	households[,c("perc_0to17","perc_18to25","perc_26to34","perc_35to44","perc_45to54","perc_55to64","perc_65plus")] <- 0	
	households[names(number_0to17),"perc_0to17"] <- number_0to17/households[names(number_0to17),"household_size"] 
	households[names(number_18to25),"perc_18to25"] <- number_18to25/households[names(number_18to25),"household_size"] 
	households[names(number_26to34),"perc_26to34"] <- number_26to34/households[names(number_26to34),"household_size"] 
	households[names(number_35to44),"perc_35to44"] <- number_35to44/households[names(number_35to44),"household_size"] 
	households[names(number_45to54),"perc_45to54"] <- number_45to54/households[names(number_45to54),"household_size"] 
	households[names(number_55to64),"perc_55to64"] <- number_55to64/households[names(number_55to64),"household_size"] 
	households[names(number_65plus),"perc_65plus"] <- number_65plus/households[names(number_65plus),"household_size"] 
				
	# Race groups	
	acs[acs$RACE == 1,"RACE"] <- "White"
	acs[acs$RACE == 2,"RACE"] <- "Black/African American"
	acs[acs$RACE %in% c(4,5,6),"RACE"] <- "Asian"
	acs[acs$RACE %in% c(3,7,8,9),"RACE"] <- "Other Race"
	
	acs$race <- acs$RACE
	acs[acs$HISPAN %in% c(1,2,3,4),"race"] <- "Hispanic"
	
	number_white <- by(acs[acs$race == "White","race"],acs[acs$race == "White","household_year"],length)
	number_black <- by(acs[acs$race == "Black/African American","race"],acs[acs$race == "Black/African American","household_year"],length)
	number_hispanic <- by(acs[acs$race == "Hispanic","race"],acs[acs$race == "Hispanic","household_year"],length)
	number_asian <- by(acs[acs$race == "Asian","race"],acs[acs$race == "Asian","household_year"],length)
	number_other <- by(acs[acs$race %in% c("Native American","Other Race"),"race"],acs[acs$race %in% c("Native American","Other Race"),"household_year"],length)
	
	households[,c("perc_white","perc_black","perc_hispanic","perc_asian","perc_other")] <- 0	
	households[names(number_white),"perc_white"] <- number_white/households[names(number_white),"household_size"] 
	households[names(number_black),"perc_black"] <- number_black/households[names(number_black),"household_size"] 
	households[names(number_hispanic),"perc_hispanic"] <- number_hispanic/households[names(number_hispanic),"household_size"] 
	households[names(number_asian),"perc_asian"] <- number_asian/households[names(number_asian),"household_size"] 
	households[names(number_other),"perc_other"] <- number_other/households[names(number_other),"household_size"] 
	 
				
	# Income	
	acs$income <- NA
	acs[acs$FTOTINC < 25000,"income"] <- "lt25000"
	acs[acs$FTOTINC >= 25000 & acs$FTOTINC < 50000,"income"] <- "25000to50000"
	acs[acs$FTOTINC >= 50000 & acs$FTOTINC < 75000,"income"] <- "50000to75000"
	acs[acs$FTOTINC >= 75000,"income"] <- "gt75000"
	 
	# Marital Status
	acs$married <- as.numeric(acs$MARST %in% c(1,2))	
	 
	# Employed
	acs$employed <- as.numeric(acs$EMPSTAT == 1)
	 
	# Uninsured
	acs$uninsured <- 0
	acs[uninsured,"uninsured"] <- 1
	 	
##### Consider Missing FPL values and obtain imputation regression coefficients
	# NOTE: I need to do this before we start deleting records
	# What is missing?
		# In the Covered California data, a large portion of those not subsidized
		# We have income for everyone that is subsidized (as well as some that are not subsidized)
	# Exchange data:
		# Assumption: anyone who didn't apply for income assistance (and hence didn't have to report income) 
			# likely has income above 400% of FPL (or is behaving as if they think they have income > 400% of FPL)
			# Otherwise, why wouldn't they apply for advance PTCs?
		# Imputation sample: ACS households above 400% of FPL
			# Dependent variable: FPL
			# Independent variables: county, household_size, perc_male, perc_0to17, perc_18to25, perc_26to34, 
								# perc_35to44, perc_45to54, perc_55to64, perc_65plus						
		# Save results for later when we load the exchange data
	# NOTE: I need to do this before we start deleting records
	
		
		spec <- FPL ~ as.factor(rating_area) + household_size + perc_male + 
			perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 + perc_45to54 + perc_55to64 +
			perc_white + perc_black + perc_hispanic + perc_asian 
		income_OLS <- lm(spec,data=households[households$FPL > 4,])
		income_distribution <- households[households$FPL > 4,"FPL"]
	
##### Delete Records

	# We remove undocumented immigrants because they are barred from purchasing coverage in the exchange
	acs <- acs[acs$undocumented_immigrant == 0,]

	# We remove individuals with access to an employer offer
		# This is a fairly strong assumption to not include these individuals in the outside option
		# For robustness, we should make the opposite strong assumption (assume they all have unaffordable offers and can receive subsidies)
		# Ideally, we could impute what their required contribution is, but it is nearly impossible to find data on the required contribution,
		# conditional on not taking up the employer offer

	acs <- acs[acs$access_to_emp_offer == 0,]
	
	# We don't need households that don't have any uninsured
	remaining_uninsured <- by(acs[acs$uninsured == 1,"household_year"],acs[acs$uninsured == 1,"household_year"],length) 
	households$enrollees <- 0
	households[names(remaining_uninsured),"enrollees"] <- remaining_uninsured
	
	households <- households[households$enrollees != 0,]
	acs <- acs[acs$household_year %in% rownames(households),]
	households <- households[rownames(households) %in% unique(acs$household_year),] 
	
##### Determine Subsidy Eligibility
	# Income (138-400% of FPL)
		# Lawfully present immigrants who are not eligible for Medicaid can get subsidies below 138% of FPL
		# For California, this just ends up being temporary immigrants/residents 
		# For other states, there may be a waiting period for permanent residents (typically 5 years)
	# Not Medicaid/CHIP eligible
	# No affordable employer offer

	acs$subsidy_eligible <- 1
	
	# Medicaid eligibles
		# Anyone who is citizen with income below 138
		# Anyone who is a permanent resident with income below 138
		# NOTE: there is not 5-year waiting period after becoming a citizen for Medicaid eligibility in California
			# Most states have this waiting period
	
	medicaid_eligibles <- rownames(acs[acs$FPL < 1.38 & (acs$temporary_resident == 0 | is.na(acs$temporary_resident)),])
	
	# CHIP eligible (http://kff.org/health-reform/state-indicator/medicaid-and-chip-income-eligibility-limits-for-children-as-a-percent-of-the-federal-poverty-level/)
		# All but four counties: up to 266% of FPL
		# 3 counties: up to 322% of FPL (Alameda, San Francisco, San Mateo and Santa Clara)
		# 1 county: up to 411% of FPL (I couldn't find anywhere which county this is, so I'll omit this)
		# No 5-year waiting period in CA
	
	high_chip_counties <- c("ALAMEDA","SAN FRANCISCO","SAN MATEO","SANTA CLARA")
	medicaid_eligibles <- union(medicaid_eligibles,rownames(acs[acs$FPL <= 2.66 & acs$AGE <= 18 &
		(acs$temporary_resident == 0 | is.na(acs$temporary_resident)),])) 
	medicaid_eligibles <- union(medicaid_eligibles,rownames(acs[acs$FPL <= 3.22 & acs$AGE <= 18 &
		(acs$temporary_resident == 0 | is.na(acs$temporary_resident)) & acs$county %in% high_chip_counties,]))
	
	acs[medicaid_eligibles,"subsidy_eligible"] <- 0
	
	# Income too high
	acs[acs$FPL > 4,"subsidy_eligible"] <- 0
	
	# Employer offer
		# We unfortunately don't know whether the offer is unaffordable
		# If we assume that the offer is affordable, 4,000 records are deemed ineligible for subsidies (on top of those above)
		
	households$subsidized_members <- 0
	subsidized_members <- by(acs[,"subsidy_eligible"],acs[,"household_year"],sum)
	households[names(subsidized_members),"subsidized_members"] <- subsidized_members
	households[households$subsidized_members == 0,"subsidy_linear_piece"] <- NA
	
	households$unsubsidized_members <- 0
	unsubsidized_members <- by(acs[,"subsidy_eligible"],acs[,"household_year"],length) - subsidized_members
	households[names(unsubsidized_members),"unsubsidized_members"] <- unsubsidized_members
	
	acs$subsidized <- 1
	acs[acs$subsidy_eligible == 0,"subsidized"] <- 0
	acs$subsidy_eligible <- NULL
	

##### Calculate Subsidy for Those who are Eligible

	# Enrollment Year
	acs$year <- acs$YEAR
	households$year <- by(acs$year,acs$household_year,unique)[rownames(households)]

	# Upper and lower bounds of subsidy linear piece
	households$fpl_LB <- round(income_breakpoints[households$subsidy_linear_piece],digits=2)
	names(income_breakpoints) <- c("whatever",income_pieces[1:length(income_pieces)-1])
	households$fpl_UB <- round(income_breakpoints[households$subsidy_linear_piece],digits=2)
	
	# Upper and lower bounds of contribution percentage range
	households[,c("perc_LB","perc_UB")] <- NA
	
	households[households$year == 2014,"perc_LB"] <- 
		contribution_percentages[as.character(households[households$year == 2014,"fpl_LB"]),"YR2014"]
	households[households$year == 2014,"perc_UB"] <- 
		contribution_percentages[as.character(households[households$year == 2014,"fpl_UB"]),"YR2014"]	
	
	households[households$year == 2015,"perc_LB"] <- 
		contribution_percentages[as.character(households[households$year == 2015,"fpl_LB"]),"YR2015"]
	households[households$year == 2015,"perc_UB"] <- 
		contribution_percentages[as.character(households[households$year == 2015,"fpl_UB"]),"YR2015"]	
	
	households[households$year == 2016,"perc_LB"] <- 
		contribution_percentages[as.character(households[households$year == 2016,"fpl_LB"]),"YR2016"]
	households[households$year == 2016,"perc_UB"] <- 
		contribution_percentages[as.character(households[households$year == 2016,"fpl_UB"]),"YR2016"]	
	
	households[households$year == 2017,"perc_LB"] <- 
		contribution_percentages[as.character(households[households$year == 2017,"fpl_LB"]),"YR2017"]
	households[households$year == 2017,"perc_UB"] <-
		contribution_percentages[as.character(households[households$year == 2017,"fpl_UB"]),"YR2017"]	
	
	households[households$year == 2018,"perc_LB"] <- 
		contribution_percentages[as.character(households[households$year == 2018,"fpl_LB"]),"YR2018"]
	households[households$year == 2018,"perc_UB"] <- 
		contribution_percentages[as.character(households[households$year == 2018,"fpl_UB"]),"YR2018"]	
	
	households[households$year == 2019,"perc_LB"] <- 
		contribution_percentages[as.character(households[households$year == 2019,"fpl_LB"]),"YR2019"]
	households[households$year == 2019,"perc_UB"] <- 
		contribution_percentages[as.character(households[households$year == 2019,"fpl_UB"]),"YR2019"]	
	
	# Create Poverty Threshold
	households[households$year == 2014,"poverty_threshold"] <- 
		poverty_guidelines[as.character(households[households$year == 2014,"household_size"]),"YR2014"]
	households[households$year == 2015,"poverty_threshold"] <- 
		poverty_guidelines[as.character(households[households$year == 2015,"household_size"]),"YR2015"]
	households[households$year == 2016,"poverty_threshold"] <- 
		poverty_guidelines[as.character(households[households$year == 2016,"household_size"]),"YR2016"]
	households[households$year == 2017,"poverty_threshold"] <- 
		poverty_guidelines[as.character(households[households$year == 2017,"household_size"]),"YR2017"]
	households[households$year == 2018,"poverty_threshold"] <- 
		poverty_guidelines[as.character(households[households$year == 2018,"household_size"]),"YR2018"]
	households[households$year == 2019,"poverty_threshold"] <- 
		poverty_guidelines[as.character(households[households$year == 2019,"household_size"]),"YR2019"]
		
	# SLC contribution
	
	calculate_contribution <- function(data) {
		contribution <- ((data$perc_UB - data$perc_LB) * (data$FPL - data$fpl_LB)/(data$fpl_UB - data$fpl_LB) + data$perc_LB) * 
							(data$poverty_threshold/12 * data$FPL)
		return(contribution)
	}

	calculate_contribution_300to400 <- function(data) {
		contribution <- data$perc_LB * (data$poverty_threshold/12 * data$FPL)
		return(contribution)
	}
	
	households[households$subsidy_linear_piece != "300% FPL to 400% FPL" & !is.na(households$subsidy_linear_piece),"SLC_contribution"] <- 
		calculate_contribution(households[households$subsidy_linear_piece != "300% FPL to 400% FPL" & !is.na(households$subsidy_linear_piece),])
	
	households[households$subsidy_linear_piece == "300% FPL to 400% FPL" & !is.na(households$subsidy_linear_piece),"SLC_contribution"] <- 
		calculate_contribution_300to400(households[households$subsidy_linear_piece == "300% FPL to 400% FPL" & !is.na(households$subsidy_linear_piece),])
	
	acs$SLC_contribution <- households[acs$household_year,"SLC_contribution"]
	
	# SLC Premium
	
		# Rename plan_data columns
		plan_data$Year <- plan_data$ENROLLMENT_YEAR
		plan_data$Insurer <- plan_data$Issuer_Name
		plan_data$Metal_Level <- plan_data$metal_level

		plan_data$ENROLLMENT_YEAR <- NULL
		plan_data$Issuer_Name <- NULL
		plan_data$metal_level <- NULL
	
		plan_data$Insurer <- as.character(plan_data$Insurer)
		plan_data[plan_data$Insurer == "Anthem Blue Cross","Insurer"] <- "Anthem"
		plan_data[plan_data$Insurer == "Blue Shield","Insurer"] <- "Blue_Shield"
		plan_data[plan_data$Insurer == "Chinese Community","Insurer"] <- "Chinese_Community"
		plan_data[plan_data$Insurer == "Contra Costa Health Plan","Insurer"] <- "Contra_Costa"
		plan_data[plan_data$Insurer == "Health Net","Insurer"] <- "Health_Net"
		plan_data[plan_data$Insurer == "LA Care","Insurer"] <- "LA_Care"
		plan_data[plan_data$Insurer == "Western Health","Insurer"] <- "Western"
		plan_data[plan_data$Insurer == "Sharp","Insurer"] <- "SHARP"
		plan_data[plan_data$Insurer == "UnitedHealthcare","Insurer"] <- "United"
	
	

	compute_second_lowest <- function(x,counties,plans) {
		
		# Get rating area
		rating_area <- counties[x,"Rating_Area"]
		
		# Get insurers
		all_insurers <- as.vector(counties[x,setdiff(colnames(counties),"Rating_Area")])
		available_insurers <- colnames(all_insurers)[as.vector(!is.na(all_insurers[x,]))]
		
		# Now get the plans sold by these insurers in county
		available_plans <- rownames(plans[plans$Insurer %in% available_insurers & plans$Rating_Area == rating_area & plans$Metal_Level == "Silver",])
		silver_premiums <- sort(plans[available_plans,"Premium"])/1.278
											
		# There are a few plans that insurers offer in select counties only			
		
		if(length(silver_premiums) == 1) {
			return(silver_premiums[1])
		} else {
			return(silver_premiums[2])
		}
	}	
	
	plan_data$plan_unique_id <- paste(plan_data$Plan_Name,plan_data$Year,plan_data$Rating_Area,sep="_")
	rownames(plan_data) <- plan_data$plan_unique_id	
		
	counties2014$premiumSLC <- sapply(rownames(counties2014),FUN=compute_second_lowest,counties2014,plan_data[plan_data$Year == 2014,])
	counties2015$premiumSLC <- sapply(rownames(counties2015),FUN=compute_second_lowest,counties2015,plan_data[plan_data$Year == 2015,])
	counties2016$premiumSLC <- sapply(rownames(counties2016),FUN=compute_second_lowest,counties2016,plan_data[plan_data$Year == 2016,])
	counties2017$premiumSLC <- sapply(rownames(counties2017),FUN=compute_second_lowest,counties2017,plan_data[plan_data$Year == 2017,])
	counties2018$premiumSLC <- sapply(rownames(counties2018),FUN=compute_second_lowest,counties2018,plan_data[plan_data$Year == 2018,])
	counties2019$premiumSLC <- sapply(rownames(counties2019),FUN=compute_second_lowest,counties2019,plan_data[plan_data$Year == 2019,])
		
	acs$rating_factor <- NA
	acs[acs$year == 2014,"rating_factor"] <- age_rating_factors[as.character(pmin(64,acs[acs$year == 2014,"AGE"])),"Rating_Factor"]
	acs[acs$year == 2015,"rating_factor"] <- age_rating_factors[as.character(pmin(64,acs[acs$year == 2015,"AGE"])),"Rating_Factor"]
	acs[acs$year == 2016,"rating_factor"] <- age_rating_factors[as.character(pmin(64,acs[acs$year == 2016,"AGE"])),"Rating_Factor"]
	acs[acs$year == 2017,"rating_factor"] <- age_rating_factors[as.character(pmin(64,acs[acs$year == 2017,"AGE"])),"Rating_Factor"]
	acs[acs$year == 2018,"rating_factor"] <- age_rating_factors[as.character(pmin(64,acs[acs$year == 2018,"AGE"])),"Rating_Factor2018"]
	acs[acs$year == 2019,"rating_factor"] <- age_rating_factors[as.character(pmin(64,acs[acs$year == 2019,"AGE"])),"Rating_Factor2018"]
		
	acs$premiumSLC <- NA
	acs[acs$year == 2014,"premiumSLC"] <- counties2014[acs[acs$year == 2014,"county"],"premiumSLC"] * 
		acs[acs$year == 2014,"rating_factor"]
	acs[acs$year == 2015,"premiumSLC"] <- counties2015[acs[acs$year == 2015,"county"],"premiumSLC"] * 
		acs[acs$year == 2015,"rating_factor"]	
	acs[acs$year == 2016,"premiumSLC"] <- counties2016[acs[acs$year == 2016,"county"],"premiumSLC"] * 
		acs[acs$year == 2016,"rating_factor"]	
	acs[acs$year == 2017,"premiumSLC"] <- counties2017[acs[acs$year == 2017,"county"],"premiumSLC"] * 
		acs[acs$year == 2017,"rating_factor"]	
	acs[acs$year == 2018,"premiumSLC"] <- counties2018[acs[acs$year == 2018,"county"],"premiumSLC"] * 
		acs[acs$year == 2018,"rating_factor"]
	acs[acs$year == 2019,"premiumSLC"] <- counties2019[acs[acs$year == 2019,"county"],"premiumSLC"] * 
		acs[acs$year == 2019,"rating_factor"]
		
	households$premiumSLC <- by(acs[,"premiumSLC"],acs[,"household_year"],sum)[rownames(households)]
	households$rating_factor <- by(acs[,"rating_factor"],acs[,"household_year"],sum)[rownames(households)]
	
	# We have to guard against the possibility that household have both subsidized and unsubsidized members
	
		compute_unsubsidized_SLC_portion <- function(household,acs) {
			unsubsidized_members <- rownames(acs[acs$household_year == household & acs$subsidized == 0 & !is.na(acs$subsidized),])
			unsubsidized_portion <- sum(acs[unsubsidized_members,"premiumSLC"],na.rm=TRUE)
			return(unsubsidized_portion)
		}
				
		mixed_subsidy_households <- rownames(households[households$unsubsidized_members > 0 & households$subsidized_members > 0,])
		premiumSLC_unsubsidized <- sapply(mixed_subsidy_households,FUN=compute_unsubsidized_SLC_portion,
					acs[acs$household_year %in% mixed_subsidy_households,c("household_year","premiumSLC","subsidized")])
		households$premiumSLC_unsubsidized <- 0
		households[names(premiumSLC_unsubsidized),"premiumSLC_unsubsidized"] <- premiumSLC_unsubsidized

		households$subsidy <- pmax(0,(households[,"premiumSLC"] - households[,"premiumSLC_unsubsidized"]) - 
			households[,"SLC_contribution"]) 
		acs$subsidy <- households[acs$household_year,"subsidy"]	

##### Determine Mandate Exemptions
	# American Indian
	# Below filing threshold
	# Undocumented immigrants (we deleted them already)
	# No affordable offer and not eligible for Medicaid
	# NOTE: lawfully present immigrants are subject to mandate

	acs$penalty.exempt <- 0
	
	
	# American Indians
	acs[acs$RACAMIND == 2,"penalty.exempt"] <- 1
	
	# Below filing threshold
	years <- c(2014,2015,2016,2017,2018,2019)
	fields <- c("single","household_head","married","widow_wchild")
	filing_threshold <- matrix(c(10150,13050,20300,16350,
									10300,13250,20600,16600,
									10350,13350,20700,16650,
									10400,16400,20800,16750,
									12000,24000,18000,24000,
									12200,24400,18350,24400),
		length(fields),length(years),dimnames=list(fields,years))
	
	determine_single_households <- function(x) {
		if(50 %in% x) {
			return(TRUE)
		} else {
			return(FALSE)
		}
	}
	
	determine_widow_households <- function(x) {
		if(5 %in% x) {
			return(TRUE)
		} else {
			return(FALSE)
		}
	}
	
	determine_single_parent_households <- function(x) {
		if (30 %in% x | 31 %in% x | 32 %in% x | 60 %in% x | 70 %in% x) {
			return(FALSE)
		} else {
			return(TRUE)
		}
	}
	
	determine_married_households <- function(x) {
		if(20 %in% x | 21 %in% x) {
			return(TRUE)
		} else {
			return(FALSE)
		}
	}
	
	determine_married_members <- function(x) {
		if(1 %in% x | 2 %in% x | 3 %in% x) {
		#if(1 %in% x) {
			return(TRUE)
		} else {
			return(FALSE)
		}
	}
	
	single_households <- by(acs$HIURULE,acs$household_year,determine_single_households)
	widow_households <- by(acs$MARST,acs$household_year,determine_widow_households)
	single_parent_households <- by(acs$HIURULE,acs$household_year,determine_single_parent_households)
	widow_households_wchild <- widow_households & single_parent_households
	married_members <- by(acs$MARST,acs$household_year,determine_married_members)
	married_households <- by(acs$HIURULE,acs$household_year,determine_married_households)
	all_other_households <- !(single_households | widow_households_wchild | married_households | married_members)
	
	households$tax_unit_type <- NA
	households[names(single_households)[single_households==TRUE],"tax_unit_type"] <- "single"
	households[names(widow_households_wchild)[widow_households_wchild==TRUE],"tax_unit_type"] <- "widow_wchild"
	households[names(married_households)[married_households==TRUE],"tax_unit_type"] <- "married"
	households[names(married_members)[married_members==TRUE],"tax_unit_type"] <- "married"
	households[names(all_other_households)[all_other_households==TRUE],"tax_unit_type"] <- "household_head"
	
	households$filing_threshold <- NA
	households[households$year == 2014,"filing_threshold"] <- 
		filing_threshold[households[households$year == 2014,"tax_unit_type"],"2014"]
	households[households$year == 2015,"filing_threshold"] <- 
		filing_threshold[households[households$year == 2015,"tax_unit_type"],"2015"]
	households[households$year == 2016,"filing_threshold"] <- 
		filing_threshold[households[households$year == 2016,"tax_unit_type"],"2016"]
	households[households$year == 2017,"filing_threshold"] <- 
		filing_threshold[households[households$year == 2017,"tax_unit_type"],"2017"]
	households[households$year == 2018,"filing_threshold"] <- 
		filing_threshold[households[households$year == 2018,"tax_unit_type"],"2018"]
	households[households$year == 2019,"filing_threshold"] <- 
		filing_threshold[households[households$year == 2019,"tax_unit_type"],"2019"]
	
	
	households_below_filing_threshold <- rownames(households[households$FPL * households$poverty_threshold < households$filing_threshold,])
	acs[acs$household_year %in% households_below_filing_threshold,"penalty.exempt"] <- 1
	
	# No Affordable Offer and Not Eligible for Medicaid
		# We can't really check the employer offer
		# We can check the exchange offer (compare income to cost of bronze plan)
		# In 2014, the exempt threshold is 8% (I think it is 8.05% in 2015, 8.13% in 2016, 8.16% in 2017, 8.05% in 2018, 8.3% in 2019)
	
	compute_cheapest_bronze <- function(x,counties,plans) {
		
		# Get rating area
		rating_area <- counties[x,"Rating_Area"]
		
		# Get insurers
		all_insurers <- as.vector(counties[x,setdiff(colnames(counties),"Rating_Area")])
		available_insurers <- colnames(all_insurers)[as.vector(!is.na(all_insurers[x,]))]
		
		# Now get the plans sold by these insurers in county
		available_plans <- rownames(plans[plans$Insurer %in% available_insurers & plans$Rating_Area == rating_area & plans$Metal_Level == "Bronze",])
		bronze_premiums <- sort(plans[available_plans,"Premium"])/1.278
											
		return(bronze_premiums[1])
	}	
	
	counties2014$cheapest_premium <- sapply(rownames(counties2014),FUN=compute_cheapest_bronze,counties2014,plan_data[plan_data$Year == 2014,])
	counties2015$cheapest_premium <- sapply(rownames(counties2015),FUN=compute_cheapest_bronze,counties2015,plan_data[plan_data$Year == 2015,])
	counties2016$cheapest_premium <- sapply(rownames(counties2016),FUN=compute_cheapest_bronze,counties2016,plan_data[plan_data$Year == 2016,])
	counties2017$cheapest_premium <- sapply(rownames(counties2017),FUN=compute_cheapest_bronze,counties2017,plan_data[plan_data$Year == 2017,])
	counties2018$cheapest_premium <- sapply(rownames(counties2018),FUN=compute_cheapest_bronze,counties2018,plan_data[plan_data$Year == 2018,])
	counties2019$cheapest_premium <- sapply(rownames(counties2019),FUN=compute_cheapest_bronze,counties2019,plan_data[plan_data$Year == 2019,])
	
	acs$cheapest_premium <- NA
	acs[acs$year == 2014,"cheapest_premium"] <- counties2014[acs[acs$year == 2014,"county"],"cheapest_premium"] * 
		acs[acs$year == 2014,"rating_factor"]
	acs[acs$year == 2015,"cheapest_premium"] <- counties2015[acs[acs$year == 2015,"county"],"cheapest_premium"] * 
		acs[acs$year == 2015,"rating_factor"]
	acs[acs$year == 2016,"cheapest_premium"] <- counties2016[acs[acs$year == 2016,"county"],"cheapest_premium"] * 
		acs[acs$year == 2016,"rating_factor"]
	acs[acs$year == 2017,"cheapest_premium"] <- counties2017[acs[acs$year == 2017,"county"],"cheapest_premium"] * 
		acs[acs$year == 2017,"rating_factor"]
	acs[acs$year == 2018,"cheapest_premium"] <- counties2018[acs[acs$year == 2018,"county"],"cheapest_premium"] * 
		acs[acs$year == 2018,"rating_factor"]
	acs[acs$year == 2019,"cheapest_premium"] <- counties2019[acs[acs$year == 2019,"county"],"cheapest_premium"] * 
		acs[acs$year == 2019,"rating_factor"]
	
	acs[medicaid_eligibles,"cheapest_premium"] <- 0
	households$cheapest_premium <- by(acs$cheapest_premium,acs$household_year,sum)[rownames(households)]
	
	unaffordable_exchange_offer_households_subsidized_2014 <- 
		rownames(households[(households$cheapest_premium - households$subsidy) * 12 > 0.08 * households$FPL * households$poverty_threshold &
					households$subsidized_members > 0 & households$year == 2014,])
	unaffordable_exchange_offer_households_unsubsidized_2014 <- 
		rownames(households[households$cheapest_premium * 12 > 0.08 * households$FPL * households$poverty_threshold &
					households$subsidized_members == 0 & households$year == 2014,])
	
	unaffordable_exchange_offer_households_subsidized_2015 <- 
		rownames(households[(households$cheapest_premium - households$subsidy) * 12 > 0.0805 * households$FPL * households$poverty_threshold &
					households$subsidized_members > 0 & households$year == 2015,])
	unaffordable_exchange_offer_households_unsubsidized_2015 <- 
		rownames(households[households$cheapest_premium * 12 > 0.0805 * households$FPL * households$poverty_threshold &
					households$subsidized_members == 0 & households$year == 2015,])
	
	unaffordable_exchange_offer_households_subsidized_2016 <- 
		rownames(households[(households$cheapest_premium - households$subsidy) * 12 > 0.0813 * households$FPL * households$poverty_threshold &
					households$subsidized_members > 0 & households$year == 2016,])
	unaffordable_exchange_offer_households_unsubsidized_2016 <- 
		rownames(households[households$cheapest_premium * 12 > 0.0813 * households$FPL * households$poverty_threshold &
					households$subsidized_members == 0 & households$year == 2016,])
	
	unaffordable_exchange_offer_households_subsidized_2017 <- 
		rownames(households[(households$cheapest_premium - households$subsidy) * 12 > 0.0816 * households$FPL * households$poverty_threshold &
					households$subsidized_members > 0 & households$year == 2017,])
	unaffordable_exchange_offer_households_unsubsidized_2017 <- 
		rownames(households[households$cheapest_premium * 12 > 0.0816 * households$FPL * households$poverty_threshold &
					households$subsidized_members == 0 & households$year == 2017,])
	
	unaffordable_exchange_offer_households_subsidized_2018 <- 
		rownames(households[(households$cheapest_premium - households$subsidy) * 12 > 0.0805 * households$FPL * households$poverty_threshold &
					households$subsidized_members > 0 & households$year == 2018,])
	unaffordable_exchange_offer_households_unsubsidized_2018 <- 
		rownames(households[households$cheapest_premium * 12 > 0.0805 * households$FPL * households$poverty_threshold &
					households$subsidized_members == 0 & households$year == 2018,])
	
	unaffordable_exchange_offer_households_subsidized_2019 <- 
		rownames(households[(households$cheapest_premium - households$subsidy) * 12 > 0.083 * households$FPL * households$poverty_threshold &
					households$subsidized_members > 0 & households$year == 2019,])
	unaffordable_exchange_offer_households_unsubsidized_2019 <- 
		rownames(households[households$cheapest_premium * 12 > 0.083 * households$FPL * households$poverty_threshold &
					households$subsidized_members == 0 & households$year == 2019,])
	
	
	acs[!rownames(acs) %in% medicaid_eligibles &  
		acs$household_year %in% c(unaffordable_exchange_offer_households_subsidized_2014,unaffordable_exchange_offer_households_subsidized_2015,unaffordable_exchange_offer_households_subsidized_2016,
			unaffordable_exchange_offer_households_subsidized_2017,unaffordable_exchange_offer_households_subsidized_2018,unaffordable_exchange_offer_households_subsidized_2019,
			unaffordable_exchange_offer_households_unsubsidized_2014,unaffordable_exchange_offer_households_unsubsidized_2015,unaffordable_exchange_offer_households_unsubsidized_2016,
			unaffordable_exchange_offer_households_unsubsidized_2017,unaffordable_exchange_offer_households_unsubsidized_2018,
			unaffordable_exchange_offer_households_unsubsidized_2019),"penalty.exempt"] <- 1
	
	acs$penalty.exempt.belowfiling <- 0
	acs[acs$household_year %in% households_below_filing_threshold,"penalty.exempt.belowfiling"] <- 1
	
	acs$penalty.exempt.unaffordable <- 0
	acs[!acs$household_year %in% households_below_filing_threshold & !rownames(acs) %in% medicaid_eligibles &  
		acs$household_year %in% c(unaffordable_exchange_offer_households_subsidized_2014,unaffordable_exchange_offer_households_subsidized_2015,unaffordable_exchange_offer_households_subsidized_2016,
			unaffordable_exchange_offer_households_subsidized_2017,unaffordable_exchange_offer_households_subsidized_2018,unaffordable_exchange_offer_households_subsidized_2019,
			unaffordable_exchange_offer_households_unsubsidized_2014,unaffordable_exchange_offer_households_unsubsidized_2015,unaffordable_exchange_offer_households_unsubsidized_2016,
			unaffordable_exchange_offer_households_unsubsidized_2017,unaffordable_exchange_offer_households_unsubsidized_2018,
			unaffordable_exchange_offer_households_unsubsidized_2019),"penalty.exempt.unaffordable"] <- 1
	
##### Remove records not in market
	# Only the uninsured
	# No Medicaid eligibles
	# No undocumented immigrants (gone already)
	# No one with an affordable employer offer (gone already)

	acs <- acs[acs$uninsured == 1,]
	acs <- acs[!rownames(acs) %in% medicaid_eligibles,]
	#acs <- acs[!acs$household_year %in% rownames(households[households$employer_offer == 1,]),]	

	# Update Household object after paring down population	
	households <- households[rownames(households) %in% unique(acs$household_year),]
	households$enrollees <- by(acs$subsidized,acs$household_year,length)[rownames(households)]
	households$subsidized_members <- by(acs$subsidized,acs$household_year,sum)[rownames(households)]
	households[households$subsidized_members == 0,"subsidy_linear_piece"] <- NA
	households$unsubsidized_members <- households$enrollees - households$subsidized_members
	households$weight <- by(acs$PERWT,acs$household_year,sum)[rownames(households)]
	
	# Update subsidy
	households$premiumSLC <- by(acs[,"premiumSLC"],acs[,"household_year"],sum)[rownames(households)]
	households$rating_factor <- by(acs[,"rating_factor"],acs[,"household_year"],sum)[rownames(households)]
	
	mixed_subsidy_households <- rownames(households[households$unsubsidized_members > 0 & households$subsidized_members > 0,])
	premiumSLC_unsubsidized <- sapply(mixed_subsidy_households,FUN=compute_unsubsidized_SLC_portion,
				acs[acs$household_year %in% mixed_subsidy_households,c("household_year","premiumSLC","subsidized")])
	households$premiumSLC_unsubsidized <- 0
	households[names(premiumSLC_unsubsidized),"premiumSLC_unsubsidized"] <- premiumSLC_unsubsidized

	households$subsidy <- pmax(0,(households[,"premiumSLC"] - households[,"premiumSLC_unsubsidized"]) - 
		households[,"SLC_contribution"]) 
	acs$subsidy <- households[acs$household_year,"subsidy"]		
	
	# Gender - note that I used all households before, now I just use the uninsured
	households$perc_male <- by(acs$gender,acs$household_year,mean)
	
	# Age groups - note that I used all households before, now I just use the uninsured
	
	number_0to17 <- by(acs[acs$age_group == "0to17","age_group"],acs[acs$age_group == "0to17","household_year"],length)
	number_18to25 <- by(acs[acs$age_group == "18to25","age_group"],acs[acs$age_group == "18to25","household_year"],length)
	number_26to34 <- by(acs[acs$age_group == "26to34","age_group"],acs[acs$age_group == "26to34","household_year"],length)
	number_35to44 <- by(acs[acs$age_group == "35to44","age_group"],acs[acs$age_group == "35to44","household_year"],length)
	number_45to54 <- by(acs[acs$age_group == "45to54","age_group"],acs[acs$age_group == "45to54","household_year"],length)
	number_55to64 <- by(acs[acs$age_group == "55to64","age_group"],acs[acs$age_group == "55to64","household_year"],length)
	number_65plus <- by(acs[acs$age_group == "65plus","age_group"],acs[acs$age_group == "65plus","household_year"],length)
		
	households[,c("perc_0to17","perc_18to25","perc_26to34","perc_35to44","perc_45to54","perc_55to64","perc_65plus")] <- 0	
	households[names(number_0to17),"perc_0to17"] <- as.numeric(number_0to17/households[names(number_0to17),"enrollees"])
	households[names(number_18to25),"perc_18to25"] <- number_18to25/households[names(number_18to25),"enrollees"] 
	households[names(number_26to34),"perc_26to34"] <- number_26to34/households[names(number_26to34),"enrollees"] 
	households[names(number_35to44),"perc_35to44"] <- number_35to44/households[names(number_35to44),"enrollees"] 
	households[names(number_45to54),"perc_45to54"] <- number_45to54/households[names(number_45to54),"enrollees"] 
	households[names(number_55to64),"perc_55to64"] <- number_55to64/households[names(number_55to64),"enrollees"] 
	households[names(number_65plus),"perc_65plus"] <- number_65plus/households[names(number_65plus),"enrollees"] 
	
	# Race groups - note that I used all households before, now I just use the uninsured
	
	number_white <- by(acs[acs$race == "White","race"],acs[acs$race == "White","household_year"],length)
	number_black <- by(acs[acs$race == "Black/African American","race"],acs[acs$race == "Black/African American","household_year"],length)
	number_hispanic <- by(acs[acs$race == "Hispanic","race"],acs[acs$race == "Hispanic","household_year"],length)
	number_asian <- by(acs[acs$race == "Asian","race"],acs[acs$race == "Asian","household_year"],length)
	number_other <- by(acs[acs$race %in% c("Native American","Other Race"),"race"],acs[acs$race %in% c("Native American","Other Race"),"household_year"],length)
	
	households[,c("perc_white","perc_black","perc_hispanic","perc_asian","perc_other")] <- 0	
	households[names(number_white),"perc_white"] <- number_white/households[names(number_white),"household_size"] 
	households[names(number_black),"perc_black"] <- number_black/households[names(number_black),"household_size"] 
	households[names(number_hispanic),"perc_hispanic"] <- number_hispanic/households[names(number_hispanic),"household_size"] 
	households[names(number_asian),"perc_asian"] <- number_asian/households[names(number_asian),"household_size"] 
	households[names(number_other),"perc_other"] <- number_other/households[names(number_other),"household_size"] 
	 
	
	
	# Plan info
	
	#plan_variables <- c("metal_enhanced","insurer","plan_unique_id","METAL_LEVEL","plan_name","start_date","end_date","premium21")
	#acs[,plan_variables] <- NA	
	
	
##### Get rid of variables we don't need
to.delete <- c("uninsured_weight","members_ESI","problem_county","PUMA","employer_offer","combination","unsubsidized_members")
households <- households[,!colnames(households) %in% to.delete] 
	
to.delete <- c("DATANUM","SERIAL","HHWT","STATEFIP","COUNTY","COUNTYFIPS","PUMA","GQ", "NFAMS","NSUBFAM",
				"PERNUM","FAMSIZE","FAMUNIT","SUBFAM","SFTYPE","SFRELATE","CBSUBFAM","RELATE",
				"RELATED","RACED","CITIZEN","YRNATUR","YRIMMIG","YRSUSA1","YRSUSA2","RACESING",
				"RACESINGD","RACAMIND","HCOVANY","HCOVPRIV","HINSEMP","HINSPUR","HINSTRI","HCOVPUB",
				"HINSCAID","HINSCARE","HINSVA","HINSIHS","HIUFPGBASE","HIUFPGINC","HIURULE","HIUID",
				"HIUNPERS","INCTOT","FTOTINC","undocumented_immigrant","YEAR","OWNERSHP",
				"OWNERSHPD","MARST","HISPAN","HISPAND","BPL","BPLD","SPEAKENG","EDUC","poverty_threshold",
				"EDUCD","EMPSTAT","EMPSTATD","IND","IND1950","IND1990","UHRSWORK","immigration_id",
				"permanent_resident","temporary_resident","imputed_offer","ESI","access_to_emp_offer",
				"education","income","married","employed","uninsured","age_group","RACE")
acs <- acs[,!colnames(acs) %in% to.delete]	

##### Renumber ids to be consistent with exchange data
			
	# Individual ID 
		# start at 100000000 to deconflict with exchange data
		# note that the acs is not a panel
		
	acs$individual_id <- 1:dim(acs)[1] + 100000000	
	gc()
	
	
	
##### Merge ACS data in with Exchange data

data <- get(load("data/final/enroll_temp")) # "Cleaned" Covered California enrollment data
h <- households # I can't figure out why households gets overwritten when you read in exchange_households
exchange_households <- get(load("data/final/household_temp")) # "Cleaned" Covered California household data

rm(households)
gc()

	# Functions
	
	get_available_plans <- function(i,zip3_choices,bronze_flag) {
		
			if(bronze_flag) {
				region_year_plans <- which(plan_data$Year == zip3_choices[i,"Year"] &
					plan_data$region == zip3_choices[i,"Region"] & plan_data$Metal_Level == "Bronze")
			} else {
				region_year_plans <- which(plan_data$Year == zip3_choices[i,"Year"] &
					plan_data$region == zip3_choices[i,"Region"])
			}
			
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
			return(available_plans)
		}
	
		compute_cheapest_bronze_ex <- function(i,zip3_choices) {
			available_plans <- get_available_plans(i,zip3_choices,TRUE)
			cheapest_bronze_premium <- min(plan_data[available_plans,"Premium"]/1.278)
			return(cheapest_bronze_premium)	
		}	
		
		
		single_product_insurers <- c("Chinese_Community","Contra_Costa","Kaiser","LA_Care","Molina",
				"Oscar","United","Valley","Western")
		zipchoices <- as.matrix(zip3_choices[,4:ncol(zip3_choices)])
	
	# Rename variables
		
		
		exchange_households$household_size <- exchange_households$implied_household_size		
		
		#exchange_households$household_size <- exchange_households$members
		exchange_households$enrollees <- exchange_households$members
		exchange_households$members <- NULL
		exchange_households$implied_household_size <- NULL
		
		household_rating_factors <- by(data[,"rating_factor"],data[,"household_year"],sum,na.rm=TRUE)
		exchange_households[names(household_rating_factors),"rating_factor"]	<- household_rating_factors
		
		exchange_households$rating_area <- exchange_households$region
		exchange_households$region <- NULL
		data$rating_area <- data$region
		data$region <- NULL	

		exchange_households$FPL <- exchange_households$FPL/100
		data$FPL <- data$FPL/100
		
	# Create subsample for dynamic analysis
	
			# You need an I by T object with each household's choice
			ex_household_ids <- by(data$household_id, data$household_year,unique)
			exchange_households[names(ex_household_ids),"household_id"] <- ex_household_ids 
			household_names <- unique(exchange_households$household_id)
			years <- c(2014:2019)
			new_fields <- paste("choice",years,sep="_")
			dynamic_choices <- matrix(NA,length(household_names),length(years), dimnames=list(household_names,years))
	
			# Insert unique plan name into data object
			data$plan_unique_id <- NA
			data[!is.na(data$plan_id),"plan_unique_id"] <- plan_data[data[!is.na(data$plan_id),"plan_id"],"plan_unique_id"]
	
			# Assign an integer to each plan
			plan_names <- sort(unique(as.character(plan_data$Plan_Name2)))
			plan_numbers <- 1:length(plan_names)
			names(plan_numbers) <- plan_names
			data$plan_number <- NA
			data[!is.na(data$plan_id),"plan_number"] <- plan_numbers[data[!is.na(data$plan_id),"plan_name"]]
			plan_data$Plan_Name2 <- as.character(plan_data$Plan_Name2)
			plan_data$plan_number <- plan_numbers[plan_data$Plan_Name2]
			
			# Assign an integer for the no CSR version of the plan
			data$plan_name_nocsr <- plan_data[data$plan_id,"Plan_Name2_NOCSR"]
			plan_names_nocsr <- sort(unique(as.character(plan_data$Plan_Name2_NOCSR)))
			plan_numbers_nocsr <- plan_numbers[plan_names_nocsr]
			names(plan_numbers_nocsr) <- plan_names_nocsr
			data$plan_number_nocsr <- NA
			data[!is.na(data$plan_id),"plan_number_nocsr"] <- plan_numbers_nocsr[data[!is.na(data$plan_id),"plan_name_nocsr"]]
			plan_data$Plan_Name2_NOCSR <- as.character(plan_data$Plan_Name2_NOCSR)
			plan_data$plan_number_nocsr <- plan_numbers[plan_data$Plan_Name2_NOCSR]
			

			# Assign an integer to each plan (eliminating small plans
			plan_names_small <- sort(unique(plan_data$Plan_Name_Small))
			plan_numbers_small <- 1:length(plan_names_small)
			names(plan_numbers_small) <- plan_names_small
			data$plan_number_small <- NA
			data[!is.na(data$plan_id),"plan_number_small"] <- plan_numbers_small[data[!is.na(data$plan_id),"plan_name"]]
			plan_data$Plan_Name_Small <- as.character(plan_data$Plan_Name_Small)
			plan_data$plan_number_small <- plan_numbers_small[plan_data$Plan_Name_Small]
	
			assign_head_plan <- function(x) {
				if(length(x[!is.na(x)]) == 0) {
					return(NA)
				} else {
					return(as.integer(x[!is.na(x)][1]))
				}
			}
			
			assign_oldest <- function(x) {
				if(length(x[!is.na(x)]) == 0) {
					return(NA)
				} else {
					return(as.character(x[!is.na(x)][1]))
				}
			}
			
			
			exchange_households[,"plan_id"] <- 
				as.numeric(by(data[,"plan_id"],data[,"household_year"],assign_head_plan)[rownames(exchange_households)])
			exchange_households[,"plan_name"] <- 
				by(data[,"plan_name"],data[,"household_year"],assign_oldest)[rownames(exchange_households)]
			exchange_households[,"plan_unique_id"] <- 
				by(data[,"plan_unique_id"],data[,"household_year"],assign_oldest)[rownames(exchange_households)]
			
			
			exchange_households$previous_plan_number <- NA
			exchange_households$next_plan_number <- NA
			exchange_households$previous_plan_offered <- NA
			exchange_households$zip_region_year <- paste(exchange_households$zip3,exchange_households$rating_area,exchange_households$year,sep="_")		
			exchange_households$plan_number_nocsr <- as.numeric(by(data[,"plan_number_nocsr"],data[,"household_year"],assign_head_plan)[rownames(exchange_households)])
			for(t in years) {
				household_names_t <- as.character(exchange_households[exchange_households$year == t & !exchange_households$flagged,"household_id"])				
				plan_numbers_year <- as.numeric(exchange_households[exchange_households$year == t & !exchange_households$flagged,"plan_number_nocsr"])
				indices <- which(rownames(dynamic_choices) %in% household_names_t)
				dynamic_choices[indices,as.character(t)] <- plan_numbers_year
				
				if(t > min(years)) {
					enrolled_previous_year <- which(exchange_households$household_id %in% as.numeric(household_names_t) & exchange_households$year == t-1 & !exchange_households$flagged)
					plan_numbers_previous_year <- exchange_households[enrolled_previous_year,"plan_number_nocsr"]
					current_year_identifiers <- paste(exchange_households[enrolled_previous_year,"household_id"],t,sep="_")
					exchange_households[which(rownames(exchange_households) %in% current_year_identifiers),"previous_plan_number"] <-  plan_numbers_previous_year
					
					for(i in rownames(zip3_choices[zip3_choices$Year == t,])) {
						available_plans <- unique(plan_numbers_nocsr[plan_data[get_available_plans(i,zip3_choices,FALSE),"Plan_Name2_NOCSR"]])
						enrolled_previous_year_zipregionyear <- intersect(which(rownames(exchange_households) %in% current_year_identifiers),
							which(exchange_households$zip_region_year == i))
						previous_plans <- exchange_households[enrolled_previous_year_zipregionyear,"previous_plan_number"]
						exchange_households[enrolled_previous_year_zipregionyear,"previous_plan_offered"] <- as.numeric(previous_plans %in% available_plans)
					}
				}	
				
				if(t < max(years)) {
					enrolled_next_year <- which(exchange_households$household_id %in% as.numeric(household_names_t) & exchange_households$year == t+1 & !exchange_households$flagged)
					plan_numbers_next_year <- exchange_households[enrolled_next_year,"plan_number_nocsr"]
					current_year_identifiers <- paste(exchange_households[enrolled_next_year,"household_id"],t,sep="_")
					exchange_households[which(rownames(exchange_households) %in% current_year_identifiers),"next_plan_number"] <-  plan_numbers_next_year
				}
			
			}
		
			data[is.na(data$plan_id),"plan_number"] <- max(data$plan_number,na.rm=TRUE) + 1
			data[is.na(data$plan_id),"plan_number_nocsr"] <- max(data$plan_number_nocsr,na.rm=TRUE) + 1
			data[is.na(data$plan_id),"plan_number_small"] <- max(data$plan_number_small,na.rm=TRUE) + 1
		
		save(dynamic_choices,file="data/final/dynamic_choices")
		rm(dynamic_choices)
		gc()
		
		# Enrollment Timing Variables
			# Enrolled in previous year
			# Enrolled at beginning of year
			# Enrolled SEP
			# Departed midyear
		
			# Precendence Rules: if anyone enrolled previous year, whole household is enrolled previous year
				# Then enrolled at beginning of year
				# Then SEP
	
			
				# Departed Midyear
					# Anyone who dropped out before Oct. 1 
					# I'm using the 90-day grace period rule
					
				data$dep_midyear <- as.numeric(data$end_week < 39 & !is.na(data$end_week))
				exchange_households$dep_midyear <- NA
				midyear_departures <- by(data[,"dep_midyear"],data[,"household_year"],sum,na.rm=TRUE)
				exchange_households[names(midyear_departures),"dep_midyear"] <- midyear_departures
				exchange_households$dep_midyear <- as.numeric(exchange_households$dep_midyear >= exchange_households$enrollees)
			
				# SEP
				exchange_households$SEP <- NA
				SEP_enrollees <- by(as.numeric(data[,"OEP"]),data[,"household_year"],sum,na.rm=TRUE)
				exchange_households[names(SEP_enrollees),"SEP"] <- SEP_enrollees
				exchange_households$SEP <- as.numeric(exchange_households$SEP < exchange_households$enrollees)
				
		# Demographic variables
		
			# Gender
			data$gender <- as.numeric(data$gender)
			exchange_households$perc_male <- by(data$gender,data$household_year,mean,na.rm=TRUE)
			
				# Drop households with NA (only 4 households)
				keep_households <- rownames(exchange_households[!is.na(exchange_households$perc_male),])
				exchange_households <- exchange_households[keep_households,]
				data <- data[data$household_year %in% keep_households,]	
			
			# Age Groups			
			number_0to17 <- by(data[data$age < 18,"age"],data[data$age < 18,"household_year"],length)
			number_18to25 <- by(data[data$age >= 18 & data$age < 26,"age"],data[data$age >= 18 & data$age < 26,"household_year"],length)
			number_26to34 <- by(data[data$age >= 26 & data$age < 35,"age"],data[data$age >= 26 & data$age < 35,"household_year"],length)
			number_35to44 <- by(data[data$age >= 35 & data$age < 45,"age"],data[data$age >= 35 & data$age < 45,"household_year"],length)
			number_45to54 <- by(data[data$age >= 45 & data$age < 55,"age"],data[data$age >= 45 & data$age < 55,"household_year"],length)
			number_55to64 <- by(data[data$age >= 55 & data$age < 65,"age"],data[data$age >= 55 & data$age < 65,"household_year"],length)
			number_65plus <- by(data[data$age >= 65,"age"],data[data$age >= 65,"household_year"],length)
			
			exchange_households[,c("perc_0to17","perc_18to25","perc_26to34","perc_35to44","perc_45to54","perc_55to64","perc_65plus")] <- 0	
			exchange_households[names(number_0to17),"perc_0to17"] <- number_0to17/exchange_households[names(number_0to17),"enrollees"] 
			exchange_households[names(number_18to25),"perc_18to25"] <- number_18to25/exchange_households[names(number_18to25),"enrollees"] 
			exchange_households[names(number_26to34),"perc_26to34"] <- number_26to34/exchange_households[names(number_26to34),"enrollees"] 
			exchange_households[names(number_35to44),"perc_35to44"] <- number_35to44/exchange_households[names(number_35to44),"enrollees"] 
			exchange_households[names(number_45to54),"perc_45to54"] <- number_45to54/exchange_households[names(number_45to54),"enrollees"] 
			exchange_households[names(number_55to64),"perc_55to64"] <- number_55to64/exchange_households[names(number_55to64),"enrollees"] 
			exchange_households[names(number_65plus),"perc_65plus"] <- number_65plus/exchange_households[names(number_65plus),"enrollees"] 
		
			# RACE
			
			number_white <- by(data[data$race == "White","race"],data[data$race == "White","household_year"],length)
			number_black <- by(data[data$race == "Black/African American","race"],data[data$race == "Black/African American","household_year"],length)
			number_hispanic <- by(data[data$race == "Hispanic","race"],data[data$race == "Hispanic","household_year"],length)
			number_asian <- by(data[data$race == "Asian","race"],data[data$race == "Asian","household_year"],length)
			number_other <- by(data[data$race %in% c("Native American","Other Race"),"race"],data[data$race %in% c("Native American","Other Race"),"household_year"],length)
			
			exchange_households[,c("perc_white","perc_black","perc_hispanic","perc_asian","perc_other")] <- 0	
			exchange_households[names(number_white),"perc_white"] <- number_white/exchange_households[names(number_white),"household_size"] 
			exchange_households[names(number_black),"perc_black"] <- number_black/exchange_households[names(number_black),"household_size"] 
			exchange_households[names(number_hispanic),"perc_hispanic"] <- number_hispanic/exchange_households[names(number_hispanic),"household_size"] 
			exchange_households[names(number_asian),"perc_asian"] <- number_asian/exchange_households[names(number_asian),"household_size"] 
			exchange_households[names(number_other),"perc_other"] <- number_other/exchange_households[names(number_other),"household_size"] 
			 
		
		
		# Impute FPL (mostly unsubsidized people) in exchange
			# I want the above 400% of FPL crowd to have a similar distribution to what we observe in the ACS
			# Use OLS model from above to rank the households by income
			# Sample the ACS income distribution and assign the income associated with their rank
			# A very small number of the missing values have incomes below 400% - just randomly sample in their relevant income range
		
			income_scores <- predict(income_OLS,newdata=exchange_households[exchange_households$subsidy_fpl_bracket == "400% FPL or greater" & 
				!exchange_households$flagged & is.na(exchange_households$FPL),])
			income_ranks <- rank(income_scores,ties.method="random")
			sampled_incomes <- sort(sample(income_distribution,size=nrow(exchange_households[exchange_households$subsidy_fpl_bracket == "400% FPL or greater"  & 
				!exchange_households$flagged & is.na(exchange_households$FPL),]),replace=TRUE))
			imputed_incomes <- sampled_incomes[income_ranks]
		
			exchange_households[exchange_households$subsidy_fpl_bracket == "400% FPL or greater" & !exchange_households$flagged & is.na(exchange_households$FPL),"FPL"] <- imputed_incomes
			
			income_groups <- c("138% FPL or less","138% FPL to 150% FPL","150% FPL to 200% FPL","200% FPL to 250% FPL","250% FPL to 400% FPL")     
			for(group in income_groups) {
				exchange_households[exchange_households$subsidy_fpl_bracket == group & is.na(exchange_households$FPL) & !exchange_households$flagged,"FPL"] <- 
					sample(exchange_households[exchange_households$subsidy_fpl_bracket == group & !is.na(exchange_households$FPL) & !exchange_households$flagged,"FPL"],
						size=length(exchange_households[exchange_households$subsidy_fpl_bracket == group & is.na(exchange_households$FPL) & !exchange_households$flagged,"FPL"]))  
			}
			
			data$FPL <- exchange_households[data$household_year,"FPL"]
			
			rm(h)
			rm(acs)
			gc()		
		
	# Add households
		
		
		# Add uninsured record for every year household is missing
			
			all_households <- unique(exchange_households$household_id)
			all_household_years <- expand.grid(as.character(all_households), as.character(years))
			rownames(all_household_years) <- paste(all_household_years[,1],all_household_years[,2],sep="_")
			colnames(all_household_years) <- c("household_id","year")
			
			all_household_years[,"household_id"] <- as.integer(as.character(all_household_years[,"household_id"]))
			all_household_years[,"year"] <- as.integer(as.character(all_household_years[,"year"]))

			add_household_years <- all_household_years[setdiff(rownames(all_household_years),rownames(exchange_households)),]
			add_household_years[,"reference_household"] <- NA
			add_household_years[,"reference_type"] <- NA			
				
			# We need to find the reference household (closest entering or departing household)
			
				# In Previous Year
				in_previous_year <- which(paste(add_household_years[,"household_id"],add_household_years[,"year"] - 1,sep="_") %in% rownames(exchange_households))
				add_household_years[in_previous_year,"reference_household"] <- paste(add_household_years[in_previous_year,"household_id"],
					add_household_years[in_previous_year,"year"] - 1,sep="_")
				add_household_years[in_previous_year,"reference_type"] <- "previous"
			
				# In Next Year
				in_next_year <- setdiff(which(paste(add_household_years[,"household_id"],add_household_years[,"year"] + 1,sep="_") %in% 
					rownames(exchange_households)),in_previous_year)
				add_household_years[in_next_year,"reference_household"] <- paste(add_household_years[in_next_year,"household_id"],
					add_household_years[in_next_year,"year"] + 1,sep="_")
				add_household_years[in_next_year,"reference_type"] <- "next"
			
				# In Two Years Ago
				in_previous_2 <- setdiff(which(paste(add_household_years[,"household_id"],add_household_years[,"year"] - 2,sep="_") %in% 
					rownames(exchange_households)),c(in_next_year,in_previous_year))
				add_household_years[in_previous_2,"reference_household"] <- paste(add_household_years[in_previous_2,"household_id"],
					add_household_years[in_previous_2,"year"] - 2,sep="_")
				add_household_years[in_previous_2,"reference_type"] <- "previous_2years"
			
				# In Two Years Hence
				in_next_2 <- setdiff(which(paste(add_household_years[,"household_id"],add_household_years[,"year"] + 2,sep="_") %in% 
					rownames(exchange_households)),c(in_next_year,in_previous_year,in_previous_2))
				add_household_years[in_next_2,"reference_household"] <- paste(add_household_years[in_next_2,"household_id"],
					add_household_years[in_next_2,"year"] + 2,sep="_")
				add_household_years[in_next_2,"reference_type"] <- "next_2years"
			
				# In Three Years Ago
				in_previous_3 <- setdiff(which(paste(add_household_years[,"household_id"],add_household_years[,"year"] - 3,sep="_") %in% 
					rownames(exchange_households)),c(in_next_year,in_previous_year,in_previous_2,in_next_2))
				add_household_years[in_previous_3,"reference_household"] <- paste(add_household_years[in_previous_3,"household_id"],
					add_household_years[in_previous_3,"year"] - 3,sep="_")
				add_household_years[in_previous_3,"reference_type"] <- "previous_3years"
			
				# In Three Years Hence
				in_next_3 <- setdiff(which(paste(add_household_years[,"household_id"],add_household_years[,"year"] + 3,sep="_") %in% 
					rownames(exchange_households)),c(in_next_year,in_previous_year,in_previous_2,in_next_2,in_previous_3))
				add_household_years[in_next_3,"reference_household"] <- paste(add_household_years[in_next_3,"household_id"],
					add_household_years[in_next_3,"year"] + 3,sep="_")
				add_household_years[in_next_3,"reference_type"] <- "next_3years"
			
				# In Four Years Ago
				in_previous_4 <- setdiff(which(paste(add_household_years[,"household_id"],add_household_years[,"year"] - 4,sep="_") %in% 
					rownames(exchange_households)),c(in_next_year,in_previous_year,in_previous_2,in_next_2,in_previous_3,in_next_3))
				add_household_years[in_previous_4,"reference_household"] <- paste(add_household_years[in_previous_4,"household_id"],
					add_household_years[in_previous_4,"year"] - 4,sep="_")
				add_household_years[in_previous_4,"reference_type"] <- "previous_4years"
			
				# In Four Years Hence
				in_next_4 <- setdiff(which(paste(add_household_years[,"household_id"],add_household_years[,"year"] + 4,sep="_") %in% 
					rownames(exchange_households)),c(in_next_year,in_previous_year,in_previous_2,in_next_2,in_previous_3,in_next_3,in_previous_4))
				add_household_years[in_next_4,"reference_household"] <- paste(add_household_years[in_next_4,"household_id"],
					add_household_years[in_next_4,"year"] + 4,sep="_")
				add_household_years[in_next_4,"reference_type"] <- "next_4years"
			
				# In Five Years Ago
				in_previous_5 <- setdiff(which(paste(add_household_years[,"household_id"],add_household_years[,"year"] - 5,sep="_") %in% 
					rownames(exchange_households)),c(in_next_year,in_previous_year,in_previous_2,in_next_2,in_previous_3,in_next_3,in_previous_4,in_next_4))
				add_household_years[in_previous_5,"reference_household"] <- paste(add_household_years[in_previous_5,"household_id"],
					add_household_years[in_previous_5,"year"] - 5,sep="_")
				add_household_years[in_previous_5,"reference_type"] <- "previous_5years"
			
				# In Five Years Hence
				in_next_5 <- setdiff(which(paste(add_household_years[,"household_id"],add_household_years[,"year"] + 5,sep="_") %in% 
					rownames(exchange_households)),c(in_next_year,in_previous_year,in_previous_2,in_next_2,in_previous_3,in_next_3,in_previous_4,in_next_4,in_previous_5))
				add_household_years[in_next_5,"reference_household"] <- paste(add_household_years[in_next_5,"household_id"],
					add_household_years[in_next_5,"year"] + 5,sep="_")
				add_household_years[in_next_5,"reference_type"] <- "next_5years"
			
			
				# Add indices
				ex_household_indices <- 1:nrow(exchange_households)
				names(ex_household_indices) <- rownames(exchange_households)
				add_household_years$reference_index <- ex_household_indices[add_household_years$reference_household] 
			
			# Create new household object
			new_households <- exchange_households[add_household_years$reference_index,]
			new_households[,c("year","reference_household","reference_type","reference_index")] <- 
				add_household_years[,c("year","reference_household","reference_type","reference_index")]
			rownames(new_households) <- paste(new_households$household_id,new_households$year,sep="_")
			
			
			new_households$zip_region_year <- paste(new_households$zip3,new_households$rating_area,new_households$year,sep="_")
			new_households$previous_plan_number <- NA
			new_households$next_plan_number <- NA
			new_households$previous_plan_offered <- NA
			new_households$plan_number_nocsr <- NA
			
			new_households[in_previous_year,"previous_plan_number"] <- 
				exchange_households[new_households[in_previous_year,"reference_index"],"plan_number_nocsr"]
			new_households[in_next_year,"next_plan_number"] <- 
				exchange_households[new_households[in_next_year,"reference_index"],"plan_number_nocsr"]
			
			for(t in years) {
				household_names_t <- as.character(new_households[new_households$year == t & !new_households$flagged,"household_id"])				
				
				if(t > min(years)) {
					enrolled_previous_year <- which(exchange_households$household_id %in% as.numeric(household_names_t) & exchange_households$year == t-1 & !exchange_households$flagged)
					current_year_identifiers <- paste(exchange_households[enrolled_previous_year,"household_id"],t,sep="_")
					
					for(i in rownames(zip3_choices[zip3_choices$Year == t,])) {
						available_plans <- unique(plan_numbers_nocsr[plan_data[get_available_plans(i,zip3_choices,FALSE),"Plan_Name2_NOCSR"]])
						enrolled_previous_year_zipregionyear <- intersect(which(rownames(new_households) %in% current_year_identifiers),
							which(new_households$zip_region_year == i))
						previous_plans <- new_households[enrolled_previous_year_zipregionyear,"previous_plan_number"]
						new_households[enrolled_previous_year_zipregionyear,"previous_plan_offered"] <- 
							as.numeric(previous_plans %in% available_plans)
					}
				}	
			}
			
			new_households[,c("premium21","dep_midyear","SEP","metal_level_enhanced","plan_number_nocsr")] <- NA
		
		# Now create the individual records associated with new households
			
			all_data <- unique(data$individual_id)
			all_data_years <- expand.grid(as.character(all_data), as.character(years))
			rownames(all_data_years) <- paste(all_data_years[,1],all_data_years[,2],sep="_")
			colnames(all_data_years) <- c("individual_id","year")
			
			all_data_years[,"individual_id"] <- as.integer(as.character(all_data_years[,"individual_id"]))
			all_data_years[,"year"] <- as.integer(as.character(all_data_years[,"year"]))

			add_data_years <- all_data_years[setdiff(rownames(all_data_years),rownames(data)),]
			add_data_years[,"reference_individual"] <- NA
			add_data_years[,"household_year"] <- NA
			
			
			# We need to find the reference individual (closest entering or departing household)
			
				# In Previous Year
				in_previous_year <- which(paste(add_data_years[,"individual_id"],add_data_years[,"year"] - 1,sep="_") %in% rownames(data))
				add_data_years[in_previous_year,"reference_individual"] <- paste(add_data_years[in_previous_year,"individual_id"],
					add_data_years[in_previous_year,"year"] - 1,sep="_")
				add_data_years[in_previous_year,"reference_type"] <- "previous"
			
				# In Next Year
				in_next_year <- setdiff(which(paste(add_data_years[,"individual_id"],add_data_years[,"year"] + 1,sep="_") %in% 
					rownames(data)),in_previous_year)
				add_data_years[in_next_year,"reference_individual"] <- paste(add_data_years[in_next_year,"individual_id"],
					add_data_years[in_next_year,"year"] + 1,sep="_")
				add_data_years[in_next_year,"reference_type"] <- "next"
			
				# In Two Years Ago
				in_previous_2 <- setdiff(which(paste(add_data_years[,"individual_id"],add_data_years[,"year"] - 2,sep="_") %in% 
					rownames(data)),c(in_next_year,in_previous_year))
				add_data_years[in_previous_2,"reference_individual"] <- paste(add_data_years[in_previous_2,"individual_id"],
					add_data_years[in_previous_2,"year"] - 2,sep="_")
				add_data_years[in_previous_2,"reference_type"] <- "previous_2years"
			
				# In Two Years Hence
				in_next_2 <- setdiff(which(paste(add_data_years[,"individual_id"],add_data_years[,"year"] + 2,sep="_") %in% 
					rownames(data)),c(in_next_year,in_previous_year,in_previous_2))
				add_data_years[in_next_2,"reference_individual"] <- paste(add_data_years[in_next_2,"individual_id"],
					add_data_years[in_next_2,"year"] + 2,sep="_")
				add_data_years[in_next_2,"reference_type"] <- "next_2years"
			
				# In Three Years Ago
				in_previous_3 <- setdiff(which(paste(add_data_years[,"individual_id"],add_data_years[,"year"] - 3,sep="_") %in% 
					rownames(data)),c(in_next_year,in_previous_year,in_previous_2,in_next_2))
				add_data_years[in_previous_3,"reference_individual"] <- paste(add_data_years[in_previous_3,"individual_id"],
					add_data_years[in_previous_3,"year"] - 3,sep="_")
				add_data_years[in_previous_3,"reference_type"] <- "previous_3years"
			
				# In Three Years Hence
				in_next_3 <- setdiff(which(paste(add_data_years[,"individual_id"],add_data_years[,"year"] + 3,sep="_") %in% 
					rownames(data)),c(in_next_year,in_previous_year,in_previous_2,in_next_2,in_previous_3))
				add_data_years[in_next_3,"reference_individual"] <- paste(add_data_years[in_next_3,"individual_id"],
					add_data_years[in_next_3,"year"] + 3,sep="_")
				add_data_years[in_next_3,"reference_type"] <- "next_3years"
			
				# In Four Years Ago
				in_previous_4 <- setdiff(which(paste(add_data_years[,"individual_id"],add_data_years[,"year"] - 4,sep="_") %in% 
					rownames(data)),c(in_next_year,in_previous_year,in_previous_2,in_next_2,in_previous_3,in_next_3))
				add_data_years[in_previous_4,"reference_individual"] <- paste(add_data_years[in_previous_4,"individual_id"],
					add_data_years[in_previous_4,"year"] - 4,sep="_")
				add_data_years[in_previous_4,"reference_type"] <- "previous_4years"
			
				# In Four Years Hence
				in_next_4 <- setdiff(which(paste(add_data_years[,"individual_id"],add_data_years[,"year"] + 4,sep="_") %in% 
					rownames(data)),c(in_next_year,in_previous_year,in_previous_2,in_next_2,in_previous_3,in_next_3,in_previous_4))
				add_data_years[in_next_4,"reference_individual"] <- paste(add_data_years[in_next_4,"individual_id"],
					add_data_years[in_next_4,"year"] + 4,sep="_")
				add_data_years[in_next_4,"reference_type"] <- "next_4years"
			
				# In Five Years Ago
				in_previous_5 <- setdiff(which(paste(add_data_years[,"individual_id"],add_data_years[,"year"] - 5,sep="_") %in% 
					rownames(data)),c(in_next_year,in_previous_year,in_previous_2,in_next_2,in_previous_3,in_next_3,in_previous_4,in_next_4))
				add_data_years[in_previous_5,"reference_individual"] <- paste(add_data_years[in_previous_5,"individual_id"],
					add_data_years[in_previous_5,"year"] - 5,sep="_")
				add_data_years[in_previous_5,"reference_type"] <- "previous_5years"
			
				# In Five Years Hence
				in_next_5 <- setdiff(which(paste(add_data_years[,"individual_id"],add_data_years[,"year"] + 5,sep="_") %in% 
					rownames(data)),c(in_next_year,in_previous_year,in_previous_2,in_next_2,in_previous_3,in_next_3,in_previous_4,in_next_4,in_previous_5))
				add_data_years[in_next_5,"reference_individual"] <- paste(add_data_years[in_next_5,"individual_id"],
					add_data_years[in_next_5,"year"] + 5,sep="_")
				add_data_years[in_next_5,"reference_type"] <- "next_5years"
			
			
				# Add indices
				ex_individual_indices <- 1:nrow(data)
				names(ex_individual_indices) <- rownames(data)
				add_data_years$reference_index <- ex_individual_indices[add_data_years$reference_individual] 
			
				# Ages
				add_data_years$age <- data[add_data_years$reference_index,"age"]
				add_data_years[in_previous_year,"age"] <- add_data_years[in_previous_year,"age"] + 1
				add_data_years[in_previous_2,"age"] <- add_data_years[in_previous_2,"age"] + 2
				add_data_years[in_previous_3,"age"] <- add_data_years[in_previous_3,"age"] + 3
				add_data_years[in_previous_4,"age"] <- add_data_years[in_previous_4,"age"] + 4
				add_data_years[in_previous_5,"age"] <- add_data_years[in_previous_5,"age"] + 5
				
				add_data_years[in_next_year,"age"] <- add_data_years[in_next_year,"age"] - 1
				add_data_years[in_next_2,"age"] <- add_data_years[in_next_2,"age"] - 2
				add_data_years[in_next_3,"age"] <- add_data_years[in_next_3,"age"] - 3
				add_data_years[in_next_4,"age"] <- add_data_years[in_next_4,"age"] - 4
				add_data_years[in_next_5,"age"] <- add_data_years[in_next_5,"age"] - 5
				
			# Create new data object
			new_data <- data[add_data_years$reference_index,]
			new_data[,c("year","reference_individual","reference_type","reference_index","age")] <- 
				add_data_years[,c("year","reference_individual","reference_type","reference_index","age")]
			new_data$zip_region_year <- paste(new_data$zip_region,new_data$year,sep="_")
			new_data$household_year <- paste(new_data$household_id,new_data$year,sep="_")
			new_data[,c("metal_level_enhanced","insurer",
				"plan_name","premium21","plan_name_nocsr","plan_number","plan_number_nocsr","plan_number_small","plan_id","plan_unique_id",
				"OEP","dep_midyear","start_week","end_week")] <- NA
			rownames(new_data) <- paste(new_data$individual_id,new_data$year,sep="_")
			
			new_data <- new_data[new_data$household_year %in% rownames(new_households),]
			
		# Drop over 65 households from new households	
			
			over65_households <- unique(new_data[new_data$age >= 65,"household_year"]) 
			save_indices <- which(!rownames(new_households) %in% over65_households)
			new_households <- new_households[save_indices,]
			new_data <- new_data[new_data$household_year %in% rownames(new_households),]
			
		# Update Household Size
			
			new_enrollees <- by(new_data$household_year,new_data$household_year,length)
			new_households[names(new_enrollees),"enrollees"] <- new_enrollees
			new_data$members <- new_households[new_data$household_year,"enrollees"]
			
		# Infant Households
		
			infants <- which(new_data$age < 0 & new_data$members > 1)
			infant_households <- unique(new_data[infants,"household_year"])
			new_data <- new_data[setdiff(1:nrow(new_data),infants),]
			new_data$age <- pmax(0,new_data$age)
			
		# Update Household Size
			
			new_enrollees <- by(new_data$household_year,new_data$household_year,length)
			new_households[names(new_enrollees),"enrollees"] <- new_enrollees
			new_data$members <- new_households[new_data$household_year,"enrollees"]	
			
			new_households$household_size <- pmax(new_households$household_size,new_households$enrollees)
			
		# Update Variables	
			
			
			# Age Groups			
			number_0to17 <- by(new_data[new_data$age < 18,"age"],new_data[new_data$age < 18,"household_year"],length)
			number_18to25 <- by(new_data[new_data$age >= 18 & new_data$age < 26,"age"],new_data[new_data$age >= 18 & new_data$age < 26,"household_year"],length)
			number_26to34 <- by(new_data[new_data$age >= 26 & new_data$age < 35,"age"],new_data[new_data$age >= 26 & new_data$age < 35,"household_year"],length)
			number_35to44 <- by(new_data[new_data$age >= 35 & new_data$age < 45,"age"],new_data[new_data$age >= 35 & new_data$age < 45,"household_year"],length)
			number_45to54 <- by(new_data[new_data$age >= 45 & new_data$age < 55,"age"],new_data[new_data$age >= 45 & new_data$age < 55,"household_year"],length)
			number_55to64 <- by(new_data[new_data$age >= 55 & new_data$age < 65,"age"],new_data[new_data$age >= 55 & new_data$age < 65,"household_year"],length)
			number_65plus <- by(new_data[new_data$age >= 65,"age"],new_data[new_data$age >= 65,"household_year"],length)
			
			new_households[,c("perc_0to17","perc_18to25","perc_26to34","perc_35to44","perc_45to54","perc_55to64","perc_65plus")] <- 0	
			new_households[names(number_0to17),"perc_0to17"] <- number_0to17/new_households[names(number_0to17),"enrollees"] 
			new_households[names(number_18to25),"perc_18to25"] <- number_18to25/new_households[names(number_18to25),"enrollees"] 
			new_households[names(number_26to34),"perc_26to34"] <- number_26to34/new_households[names(number_26to34),"enrollees"] 
			new_households[names(number_35to44),"perc_35to44"] <- number_35to44/new_households[names(number_35to44),"enrollees"] 
			new_households[names(number_45to54),"perc_45to54"] <- number_45to54/new_households[names(number_45to54),"enrollees"] 
			new_households[names(number_55to64),"perc_55to64"] <- number_55to64/new_households[names(number_55to64),"enrollees"] 
			new_households[names(number_65plus),"perc_65plus"] <- number_65plus/new_households[names(number_65plus),"enrollees"] 
			
			# Update rating factor
			new_data$rating_factor <- age_rating_factors[as.character(pmin(64,new_data$age)),"Rating_Factor"]
			new_data[new_data$year >= 2018,"rating_factor"] <- 
				age_rating_factors[as.character(pmin(64,new_data[new_data$year >= 2018,"age"])),"Rating_Factor2018"]
			household_rating_factors <- by(new_data[,"rating_factor"],new_data[,"household_year"],sum,na.rm=TRUE)
			new_households[names(household_rating_factors),"rating_factor"]	<- household_rating_factors
			
		
			# Subsidy variables
		
			subsidy_income_range <- which(!is.na(new_households$subsidy))
		
			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2014)),"perc_LB"] <- 
				contribution_percentages[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2014)),"fpl_LB"]),"YR2014"]
			
			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2015)),"perc_LB"] <- 
				contribution_percentages[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2015)),"fpl_LB"]),"YR2015"]
		
			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2016)),"perc_LB"] <- 
				contribution_percentages[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2016)),"fpl_LB"]),"YR2016"]	

			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2017)),"perc_LB"] <- 
				contribution_percentages[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2017)),"fpl_LB"]),"YR2017"]

			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2018)),"perc_LB"] <- 
				contribution_percentages[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2018)),"fpl_LB"]),"YR2018"]
			
			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2019)),"perc_LB"] <- 
				contribution_percentages[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2019)),"fpl_LB"]),"YR2019"]
			
			
			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2014)),"perc_UB"] <- 
				contribution_percentages[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2014)),"fpl_UB"]),"YR2014"]
		
			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2015)),"perc_UB"] <- 
				contribution_percentages[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2015)),"fpl_UB"]),"YR2015"]

			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2016)),"perc_UB"] <- 
				contribution_percentages[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2016)),"fpl_UB"]),"YR2016"]
					
			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2017)),"perc_UB"] <- 
				contribution_percentages[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2017)),"fpl_UB"]),"YR2017"]
					
			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2018)),"perc_UB"] <- 
				contribution_percentages[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2018)),"fpl_UB"]),"YR2018"]
			
			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2019)),"perc_UB"] <- 
				contribution_percentages[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2019)),"fpl_UB"]),"YR2019"]
			
			
			# Poverty line
		
			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2014)),"poverty_threshold"] <- 
				poverty_guidelines[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2014)),"household_size"]),"YR2014"]
			
			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2015)),"poverty_threshold"] <- 
				poverty_guidelines[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2015)),"household_size"]),"YR2015"]
			
			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2016)),"poverty_threshold"] <- 
				poverty_guidelines[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2016)),"household_size"]),"YR2016"]
			
			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2017)),"poverty_threshold"] <- 
				poverty_guidelines[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2017)),"household_size"]),"YR2017"]
			
			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2018)),"poverty_threshold"] <- 
				poverty_guidelines[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2018)),"household_size"]),"YR2018"]

			new_households[intersect(subsidy_income_range,
					which(new_households$year == 2019)),"poverty_threshold"] <- 
				poverty_guidelines[as.character(new_households[intersect(subsidy_income_range,
					which(new_households$year == 2019)),"household_size"]),"YR2019"]
		
					
			# SLC contribution
			
			new_households[which(new_households$subsidy_linear_piece != "300% FPL to 400% FPL" & 
					!is.na(new_households$subsidy_linear_piece)),"SLC_contribution"] <- 
			calculate_contribution(new_households[
				which(new_households$subsidy_linear_piece != "300% FPL to 400% FPL" & 
					!is.na(new_households$subsidy_linear_piece)),])
						
			new_households[which(new_households$subsidy_linear_piece == "300% FPL to 400% FPL" & 
					!is.na(new_households$subsidy_linear_piece)),"SLC_contribution"] <- 
			calculate_contribution_300to400(new_households[
				which(new_households$subsidy_linear_piece == "300% FPL to 400% FPL" & 
					!is.na(new_households$subsidy_linear_piece)),])
					
			# SLC Premium
			
			get_available_silver_plans <- function(i,zip3_choices) {
		
				region_year_plans <- which(plan_data$Year == zip3_choices[i,"Year"] &
					plan_data$region == zip3_choices[i,"Region"] & plan_data$Metal_Level == "Silver")
			
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
				return(available_plans)
			}
		
			
			compute_second_lowest <- function(i,zip3_choices) {
		
				# Get available silver plans
				available_plans <- get_available_silver_plans(i,zip3_choices)
				silver_premiums <- sort(plan_data[available_plans,"Premium"]/1.278)
					
				if(length(silver_premiums) == 1) {
					return(silver_premiums[1])
				} else {
					return(silver_premiums[2])
				}
			}
		
			zip3_choices$premiumSLC <- sapply(rownames(zip3_choices),FUN=compute_second_lowest,zip3_choices)
	
			new_data$premiumSLC <- zip3_choices[new_data$zip_region_year,"premiumSLC"] * new_data$rating_factor
			household_slc_premiums <- by(new_data$premiumSLC,new_data$household_year,sum)
			new_households[names(household_slc_premiums),"premiumSLC"] <- household_slc_premiums
			new_households$subsidy <- pmax(0,new_households$premiumSLC - new_households$SLC_contribution) 
	
			# Update flagged variable
			flagged_per_household <- by(new_data$flagged,new_data$household_year,sum)
			flagged_per_household <- names(flagged_per_household[flagged_per_household > 0])
			new_households[flagged_per_household,"flagged"] <- 1

	
			# Import variables into new data
	
			to.import <- c("SLC_contribution","subsidy","premiumSLC")
			new_data[,to.import] <- new_households[new_data$household_year,to.import]
	
			to.delete <- c("reference_individual","reference_type","reference_index","plan_type","plan_mrkt")
			new_data <- new_data[,!colnames(new_data) %in% to.delete]
		
			to.delete <- c("reference_household","reference_type","reference_index")
			new_households <- new_households[,!colnames(new_households) %in% to.delete]
		
			# Delete columns in data object for memory purposes
			
			to.delete <- c("GENDER","subsidy_eligible","RES_COUNTY","Insurer","region","implied_household_size","subsidy_fpl_bracket","subsidy_linear_piece",
				"ahbx_case_id_x","AGE_BRACKET","gross_premium_amt_int","net_premium_amt_int","aptc_amt_int","PLAN_LEVEL","hios_id_14","case_year","plan_network_type",
				"old_subsidy_fpl_bracket","over25flag","members","orig_members","plan_mrkt","plan_type","zip_region","hios_id_16","race_ethnicity","ENRLEE_PLAN_SELECT_DT_WK",
				"placed_in_household","metal")
			data <- data[,!colnames(data) %in% to.delete] 
			new_data <- new_data[,!colnames(new_data) %in% to.delete] 
			
		# Merge in new data
		exchange_households <- rbind(exchange_households,new_households)
		data <- rbind(data,new_data)
	
		rm(new_households)
		rm(new_data)
		gc()
	
	# Another issue:
		# I split households that had some ununsual properties (e.g., households that selected multiple plans)
		# This causes some problems in the dynamic analysis
		# I'm going to flag anyone in one of these households
			
		individuals_in_split_households <- unique(data[data$split_flag,"individual_id"])
		split_households <- unique(data[data$individual_id %in% individuals_in_split_households,"household_id"])
		split_household_years <- expand.grid(as.character(split_households), as.character(years))
		split_household_years <- paste(split_household_years[,1],split_household_years[,2],sep="_")
			
		data[data$individual_id %in% individuals_in_split_households,"flagged"] <- TRUE
		exchange_households[intersect(split_household_years,rownames(exchange_households)),"flagged"] <- TRUE
	
		# Flag any remaining households not in data
		
		households_to_flag <- which(!(rownames(exchange_households) %in% data$household_year) & !exchange_households$flagged)
		exchange_households[households_to_flag,"flagged"] <- TRUE 
	
	# Make some fixes/additions to exchange households object 
	
		zip3_choices$cheapest_bronze <- sapply(rownames(zip3_choices),FUN=compute_cheapest_bronze_ex,zip3_choices)
	
		gc()
		
		# Create cheapest premium
		
		data$cheapest_premium <- zip3_choices[data$zip_region_year,"cheapest_bronze"] * data$rating_factor
		cheapest_premiums <- by(data$cheapest_premium,data$household_year,sum,na.rm=TRUE)
		
		exchange_households$cheapest_premium <- NA
		exchange_households[names(cheapest_premiums),"cheapest_premium"] <- cheapest_premiums
	
		# Tax unit type (this should be reviewed at some point)
		
		length_unique <- function(x) {
			return(length(unique(x)))
		}		
						
		genders_per_household <- by(data[data$age >= 26,"gender"],data[data$age >= 26,"household_year"],length_unique)
		married_households <- which(rownames(exchange_households) %in% names(genders_per_household)[genders_per_household == 2])
		single_households <- which(exchange_households$household_size == 1)
		other_households <- setdiff(1:nrow(exchange_households),c(single_households,married_households))

		exchange_households[single_households,"tax_unit_type"] <- "single"
		exchange_households[married_households,"tax_unit_type"] <- "married"
		exchange_households[other_households,"tax_unit_type"] <- "household_head"
		
		# Create filing threshold
		exchange_households$filing_threshold <- NA		
		exchange_households[exchange_households$year == 2014 & !is.na(exchange_households$year),"filing_threshold"] <- 
			filing_threshold[exchange_households[exchange_households$year == 2014 & !is.na(exchange_households$year),"tax_unit_type"],"2014"]
		exchange_households[exchange_households$year == 2015 & !is.na(exchange_households$year),"filing_threshold"] <- 
			filing_threshold[exchange_households[exchange_households$year == 2015 & !is.na(exchange_households$year),"tax_unit_type"],"2015"]
		exchange_households[exchange_households$year == 2016 & !is.na(exchange_households$year),"filing_threshold"] <- 
			filing_threshold[exchange_households[exchange_households$year == 2016 & !is.na(exchange_households$year),"tax_unit_type"],"2016"]
		exchange_households[exchange_households$year == 2017 & !is.na(exchange_households$year),"filing_threshold"] <- 
			filing_threshold[exchange_households[exchange_households$year == 2017 & !is.na(exchange_households$year),"tax_unit_type"],"2017"]
		exchange_households[exchange_households$year == 2018 & !is.na(exchange_households$year),"filing_threshold"] <- 
			filing_threshold[exchange_households[exchange_households$year == 2018 & !is.na(exchange_households$year),"tax_unit_type"],"2018"]
		exchange_households[exchange_households$year == 2019 & !is.na(exchange_households$year),"filing_threshold"] <- 
			filing_threshold[exchange_households[exchange_households$year == 2019 & !is.na(exchange_households$year),"tax_unit_type"],"2019"]

		# Poverty Threshold	
		exchange_households$poverty_threshold <- NA
		exchange_households[exchange_households$year == 2014 & !is.na(exchange_households$year),"poverty_threshold"] <- 
			poverty_guidelines[as.character(exchange_households[exchange_households$year == 2014 & !is.na(exchange_households$year),"household_size"]),"YR2014"]
		exchange_households[exchange_households$year == 2015 & !is.na(exchange_households$year),"poverty_threshold"] <- 
			poverty_guidelines[as.character(exchange_households[exchange_households$year == 2015 & !is.na(exchange_households$year),"household_size"]),"YR2015"]
		exchange_households[exchange_households$year == 2016 & !is.na(exchange_households$year),"poverty_threshold"] <- 
			poverty_guidelines[as.character(exchange_households[exchange_households$year == 2016 & !is.na(exchange_households$year),"household_size"]),"YR2016"]
		exchange_households[exchange_households$year == 2017 & !is.na(exchange_households$year),"poverty_threshold"] <- 
			poverty_guidelines[as.character(exchange_households[exchange_households$year == 2017 & !is.na(exchange_households$year),"household_size"]),"YR2017"]
		exchange_households[exchange_households$year == 2018 & !is.na(exchange_households$year),"poverty_threshold"] <- 
			poverty_guidelines[as.character(exchange_households[exchange_households$year == 2018 & !is.na(exchange_households$year),"household_size"]),"YR2018"]
		exchange_households[exchange_households$year == 2019 & !is.na(exchange_households$year),"poverty_threshold"] <- 
			poverty_guidelines[as.character(exchange_households[exchange_households$year == 2019 & !is.na(exchange_households$year),"household_size"]),"YR2019"]
			
			
		# Weight
		data$PERWT <- 1	
		households_weights <- by(data$PERWT,data$household_year,sum)
		exchange_households[names(households_weights),"weight"] <- households_weights
		

		# Delete Variables in Household Object
		to.delete <- c("ages_of_members","compositions","GROSS_PREMIUM_AMT","NET_PREMIUM_AMT","gross_premium_amt_int","net_premium_amt_int","metal_level_enhanced",
						"unsubsidized_members","plans","premium21","premiumSLC_catastrophic","region","subsidy_fpl_bracket")
		exchange_households <- exchange_households[,!colnames(exchange_households) %in% to.delete] 
		
	# Make some fixes/additions to exchange individual object before merge	
		
		# Rename Variables
		
			data$AGE <- data$age			
			data$household_size <- data$implied_household_size
		
		# Determine penalty status
		data$penalty.exempt <- 0
		data$penalty.exempt.unaffordable <- 0
		data$penalty.exempt.belowfiling <- 0
	
			# Below filing threshold
			households_below_filing_threshold <- which(exchange_households$FPL * exchange_households$poverty_threshold < 
				exchange_households$filing_threshold & !exchange_households$flagged)
			data[data$household_year %in% rownames(exchange_households)[households_below_filing_threshold] & !data$flagged,"penalty.exempt"] <- 1
	
			# No Affordable Offer 	(note, you had a major bug here before; you had 8.5% instead of 8.05%)
			affordability_threshold <- c(0.08,0.0805,0.0813,0.0816,0.0805,0.0830)
			names(affordability_threshold) <- c("2014","2015","2016","2017","2018","2019")
		
			thresholds <- rep(affordability_threshold["2014"],nrow(exchange_households))
			thresholds[which(exchange_households$year == 2015)] <- affordability_threshold["2015"]
			thresholds[which(exchange_households$year == 2016)] <- affordability_threshold["2016"]
			thresholds[which(exchange_households$year == 2017)] <- affordability_threshold["2017"]
			thresholds[which(exchange_households$year == 2018)] <- affordability_threshold["2018"]
			thresholds[which(exchange_households$year == 2019)] <- affordability_threshold["2019"]
		
			unaffordable_exchange_offer_households_subsidized <- 
				which(((exchange_households$cheapest_premium - exchange_households$subsidy) * 12 > 
					thresholds * exchange_households$FPL * exchange_households$poverty_threshold) &
						exchange_households$subsidized_members > 0 & !exchange_households$flagged)
			unaffordable_exchange_offer_households_unsubsidized <- 
				which((exchange_households$cheapest_premium * 12 > 
					thresholds * exchange_households$FPL * exchange_households$poverty_threshold )&
						exchange_households$subsidized_members == 0 & !exchange_households$flagged)
			
			data[data$household_year %in% rownames(exchange_households)[c(unaffordable_exchange_offer_households_subsidized,unaffordable_exchange_offer_households_unsubsidized)] &
				!data$flagged,"penalty.exempt"] <- 1
			
			data[data$household_year %in% rownames(exchange_households)[households_below_filing_threshold],"penalty.exempt.belowfiling"] <- 1
			data[which(data$penalty.exempt == 1 & data$penalty.exempt.belowfiling == 0),"penalty.exempt.unaffordable"] <- 1
			
			threshold.exemptions <- by(data$penalty.exempt.belowfiling,data$household_year,unique)
			unaffordable.exemptions <- by(data$penalty.exempt.unaffordable,data$household_year,max)
		
			exchange_households[names(threshold.exemptions),"exempt.belowfiling"] <- threshold.exemptions
			exchange_households[names(unaffordable.exemptions),"exempt.unaffordable"] <- unaffordable.exemptions

		to.delete <- c("age","GENDER","subsidy_eligible","RES_COUNTY","Insurer","region","implied_household_size","subsidy_fpl_bracket","subsidy_linear_piece",
			"ahbx_case_id_x","AGE_BRACKET","gross_premium_amt_int","net_premium_amt_int","aptc_amt_int","PLAN_LEVEL","hios_id_14","case_year","plan_network_type",
			"old_subsidy_fpl_bracket","over25flag","members","orig_members","plan_mrkt","plan_type","zip_region","hios_id_16","race_ethnicity","ENRLEE_PLAN_SELECT_DT_WK",
			"placed_in_household","metal")
		data <- data[,!colnames(data) %in% to.delete] 
		
		households <- exchange_households[,sort(colnames(exchange_households))]
		
		rm(exchange_households)
		gc()

	
##### Calculate Penalty
	
	# Number of Adults/Children Subject to Mandate
	households$num_children_subject <- households$num_adults_subject <- 0
	children_subject <- by(data[data$AGE < 18 & data$penalty.exempt == 0 & !data$flagged,"household_year"],data[data$AGE < 18 & data$penalty.exempt == 0 & !data$flagged,"household_year"],length)
	adults_subject <- by(data[data$AGE >= 18 & data$penalty.exempt == 0 & !data$flagged,"household_year"],data[data$AGE >= 18 & data$penalty.exempt == 0 & !data$flagged,"household_year"],length)
	households[names(children_subject),"num_children_subject"] <- children_subject
	households[names(adults_subject),"num_adults_subject"] <- adults_subject
	
	# Penalty Levels - penalty set to zero starting in 2019 (GOP tax bill)
	flat_amount_penalty <- c(95,325,695,695,695,0)
	perc_penalty <- c(0.01,0.02,0.025,0.025,0.025,0)
	names(flat_amount_penalty) <- names(perc_penalty) <- c("2014","2015","2016","2017","2018","2019")
	
	# Penalty Cap (National average bronze premium calculated by IRS)
	penalty_cap <- c(204,207,223,272,283,0) * 12
	names(penalty_cap) <- c("2014","2015","2016","2017","2018","2019")
	
	# Penalty (see slide 24 of your presentation for Bob's class)
	
		######  This needs to be reviewed at some point!!!!!!! (b/c of those for whom we do not know income and the filing threshold)

	flat_amounts <- rep(flat_amount_penalty["2014"],nrow(households))
	flat_amounts[which(households$year == 2015)] <- flat_amount_penalty["2015"]
	flat_amounts[which(households$year == 2016)] <- flat_amount_penalty["2016"]
	flat_amounts[which(households$year == 2017)] <- flat_amount_penalty["2017"]
	flat_amounts[which(households$year == 2018)] <- flat_amount_penalty["2018"]
	flat_amounts[which(households$year == 2019)] <- flat_amount_penalty["2019"]
	
	perc_penalties <- rep(perc_penalty["2014"],nrow(households))
	perc_penalties[which(households$year == 2015)] <- perc_penalty["2015"]
	perc_penalties[which(households$year == 2016)] <- perc_penalty["2016"]
	perc_penalties[which(households$year == 2017)] <- perc_penalty["2017"]
	perc_penalties[which(households$year == 2018)] <- perc_penalty["2018"]
	perc_penalties[which(households$year == 2019)] <- perc_penalty["2019"]
	
	penalty_caps <- rep(penalty_cap["2014"],nrow(households))
	penalty_caps[which(households$year == 2015)] <- penalty_cap["2015"]	
	penalty_caps[which(households$year == 2016)] <- penalty_cap["2016"]	
	penalty_caps[which(households$year == 2017)] <- penalty_cap["2017"]	
	penalty_caps[which(households$year == 2018)] <- penalty_cap["2018"]	
	penalty_caps[which(households$year == 2019)] <- penalty_cap["2019"]	
		
		
	households$penalty <- NA
	households[!households$flagged,"penalty"] <- pmin(pmax(pmin((households[!households$flagged,"num_adults_subject"] + 0.5 * households[!households$flagged,"num_children_subject"]) * 
							flat_amounts[!households$flagged],3 * flat_amounts[!households$flagged]), 
							perc_penalties[!households$flagged] * (households[!households$flagged,"FPL"] * households[!households$flagged,"poverty_threshold"] - households[!households$flagged,"filing_threshold"])), 
							(households[!households$flagged,"num_adults_subject"] + households[!households$flagged,"num_children_subject"]) * penalty_caps[!households$flagged])

##### Now let's set up the broker/navigator variables

	# Broker and Navigator variables
	data$agent <- as.numeric(data$service_channel %in% c("CIA") & !is.na(data$plan_name))
	data$broker <- as.numeric(data$service_channel %in% c("CIA","PBE") & !is.na(data$plan_name))
	data$navigator <- as.numeric(data$service_channel %in% c("SCR","CEW","CEC") & !is.na(data$plan_name))
	
	agent_assisted <- by(data[!data$flagged,"agent"],data[!data$flagged,"household_year"],max,na.rm=TRUE)
	broker_assisted <- by(data[!data$flagged,"broker"],data[!data$flagged,"household_year"],max,na.rm=TRUE)
	navigator_assisted <- by(data[!data$flagged,"navigator"],data[!data$flagged,"household_year"],max,na.rm=TRUE)
	
	households$agent <- households$broker <- households$navigator <- NA
	households[names(agent_assisted),"agent"] <- agent_assisted
	households[names(broker_assisted),"broker"] <- broker_assisted
	households[names(navigator_assisted),"navigator"] <- navigator_assisted
	
	households$forward_agent <- households$forward_broker <- households$forward_navigator <- 0
	households$ever_agent <- households$ever_broker <- households$ever_navigator <- 0
	for(t in years) {
		
		households_agent_assisted <- households[households$year == t & 
			households$agent == 1 & !is.na(households$agent) & !households$flagged,"household_id"]	
		households_broker_assisted <- households[households$year == t & 
			households$broker == 1 & !is.na(households$broker) & !households$flagged,"household_id"]	
		households_navigator_assisted <- households[households$year == t & 
			households$navigator == 1 & !is.na(households$navigator) & !households$flagged,"household_id"]	
		
		if(t < max(years)) { 
			households[households$household_id %in% households_agent_assisted & households$year >= t & !is.na(households$year),"forward_agent"] <- 1
			households[households$household_id %in% households_broker_assisted & households$year >= t & !is.na(households$year),"forward_broker"] <- 1
			households[households$household_id %in% households_navigator_assisted & households$year >= t & !is.na(households$year),"forward_navigator"] <- 1
		}
		
		households[households$household_id %in% households_agent_assisted,"ever_agent"] <- 1
		households[households$household_id %in% households_broker_assisted,"ever_broker"] <- 1
		households[households$household_id %in% households_navigator_assisted,"ever_navigator"] <- 1		
	}
		
# Let's make sure flagged records in individual and household objects are consistent
flagged_per_household <- by(data$flagged,data$household_year,sum)
flagged_per_household <- names(flagged_per_household[flagged_per_household > 0])
households[flagged_per_household,"flagged"] <- 1
data$flagged <- households[data$household_year,"flagged"]
rm(flagged_per_household)		
		

##### List of Issues with the Code Above
	# 2) Penalty exemptions and amount
		# Filing threshold for exchange enrollees (don't have marital status)
		# Exact FPL for unsubsidized exchange enrollees is unknown 
							
##### Save the data objects 
save(data,file="data/final/enroll_data")	
save(households,file="data/final/household_data")	
save(plan_data,file="data/final/plan_data")
		