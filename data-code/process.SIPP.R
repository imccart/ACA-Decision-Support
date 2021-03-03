#################################
### File to Process SIPP Data ###
#################################

# The purpose of using the SIPP is to impute undocumented immigrants
# in the ACS.  The SIPP is the only national-level survey that has
# this level of detail on immigrants.  When imputing, we have to make 
# sure that the variables in the SIPP and ACS match.

##### SIPP Variables
	# SSUID - Sample unit identifier (household id)
	# EPPPNUM - Person number (unique within sample unit)
	# TFIPSST - FIPS state code
	# ERACE - Race (1 White, 2 Black, 3 Asian, 4 Other - there could be a 3 here as America Indian, 4 Asian, 5 Other)
	# ESEX - 1 male, 2 female
	# EORIGIN - 1 Hispanic, 2 not
	# ENATCIT - how response became a US citizen
	# WPFINWGT - person weight
	# ERRP - Household relationship
	# TAGE - age
	# EMS - marital status
	# EPNSPOUS - person number of spouse (9999 - not married)
	# EEDUCATE - highest grade/degree completed
	# TBRSTATE - state or country of birth
	# ECITIZNT - citizenship status (1 yes, 2 no)
	# TIMSTAT - immigration status upon entry (1 - permanent resident, 2 - other
	# EADJUST = whether status changed to permanent resident (1 yes, 2 no)
	# TADYEAR - year status changed to permanent resident
	# TMOVEUS - year moved to the US
	# THTOTINC - total household income
	# TFTOTINC - total family income
	# TFSSI - Total family SSI
	# TFSOCSEC - total family social security
	# TFAFDC - total family public assistance payments (AFDC, TANF)
	# TFVETS - total veterans payments
	# TFFDSTP - total family food stamps
	# RHPOV - poverty threshold for household in interview month
	# ESSSELF - receipt of social security for self
	# ESSISELF - receipt of SSI for self 
	# RCUTYP01 - Social security coverage flag (1 yes 2 no)
	# RCUOWN01 - person number of the owner of SS coverage
	# RCUTYP03 - Federal SSI coverage flag
	# RCUOWN03 - person number of the owner of SSI coverage
	# RCUTYP08 - veteran payment coverage flag
	# RCUOWN8A, RCUOOWN8B
	# RCUTYP20 - public assistance payment program coverage flag (AFDC/TANF)
	# RCUOWN20
	# RCUTYP25 - WIC coverage flag
	# RCUOWN25
	# RCUTYP27 - Food stamp coverage flag
	# RCUOWN27
	# RCUTYP58 - health insurance coverage flag (not Medicare or Medicaid)
	# RENROLL - enrolled in school full or part-time
	# EENLEVEL - level or grade enrolled
	# EASST01 - received federal pell grant
	# EWICYN = received WIC
	# ECRMTH - Medicare in current month
	# ECDMTH - Medicaid in current month
	# EMCOCOV - type of public insurance for those under 20
	# EHIMTH - health coverage in current month
	# RCHAMPM - military-related coverage  in current month
	# EHEMPLY - source of health insurance (excluding Medicare/Medicaid)
	# EHIRSN02/EHIRSN03 - reasons for not being covered (lack of offer)
	# EPDJBTHN - did respondent have at least one job during reference period (1 yes 2 no)
	# z <-  - employment status recode for month
	# EHOWWELL - ability to speak English
	
	
# Unauthorized status:
	# Approach developed by Hall, Greenman, and Farkas (201)
	# Those classified as legal
		# Citizens
		# Entering as a legal permanent resident (TIMSTAT)
		# Status changing to permanent (EADJUST)
		# Those receiving federal welfare benefits (e.g., Food Stamps, Medicaid, SSI, TANF)
		# Respondent or spouse is a high-ranking official?
		# Respondent or spouse is enrolled in college?
	
# Predicting Unauthorized Status
	# Year of entry (duration of US residence) - TMOVEUS, YRIMMIG
	# Place of birth (world region of birth) - TBRSTATE, BPL
	# English language proficiency - EHOWWELL, SPEAKENG
	# income to poverty ratio - TFTOTINC/THTOTINC/RHPOV, POVERTY
	# Ethnicity - EORIGIN, HISPAN
	# Employment - EPDJBTHN/EJOBCNTR, EMPSTAT
	# Age - TAGE, AGE
	# Gender - ESEX, SEX
	# Race - ERACE, RACE
	# Industry - EJBIND1, IND
	# Marital status - EMS, MARST
	# Parental status - ERRP, RELATE
	# Insurance coverage - EHIMTH/ECRMTH/ECDMTH, HCOVANY
	# Educational attainment - EEDUCATE, EDUC
	# Household size - EHHNUMPP/EFNP, FAMSIZE
	# Homeownership - ETENURE, OWNERSHIP
	# US state/region of residence (may want to omit this) - TFIPSST, STATFIP
	

input <- parse.SAScii("data/ACS-SIPP/input20142.sas")
rownames(input) <- input$varname
ipums_data <- read.table(gzfile("data/ACS-SIPP/ipumsCA.gz"),skipNul = TRUE)

acs <- data.frame(matrix(NA,nrow=dim(ipums_data)[1],ncol=dim(input)[1]))
colnames(acs ) <- rownames(input)

counter <- 0
for(i in colnames(acs )) { 
	acs[,i] <- as.numeric(substr(as.character(ipums_data[,1]),start=counter+1,stop=counter+input[i,"width"]))
	counter <- counter+input[i,"width"]
}
rm(ipums_data)
gc()

##### Unzip SIPP topical module data
input <- parse.SAScii("data/ACS-SIPP/inputSIPP_tm2.sas")
rownames(input) <- input$varname
sipp_raw <- read.table(unzip("data/ACS-SIPP/p08putm2.zip"))
sipp_tm2 <- data.frame(matrix(NA,nrow=dim(sipp_raw)[1],ncol=dim(input)[1]))
colnames(sipp_tm2) <- rownames(input)
	
counter <- 0
for(i in colnames(sipp_tm2)) { 
	sipp_tm2[,i] <- as.numeric(substr(as.character(sipp_raw[,1]),start=counter+1,stop=counter+input[i,"width"]))
	counter <- counter+input[i,"width"]
}
rm(sipp_raw)
gc()

rownames(sipp_tm2) <- paste(sipp_tm2$SSUID,sipp_tm2$EPPPNUM,sep="")
to.save <- c("TBRSTATE","ECITIZNT","TIMSTAT","EADJUST","TADYEAR","TMOVEUS")
sipp_tm2 <- sipp_tm2[,to.save]	


##### Unzip SIPP core wave 2 data
input <- parse.SAScii("data/ACS-SIPP/inputSIPP_core.sas")
rownames(input) <- input$varname
sipp_raw <- read.table(unzip("data/ACS-SIPP/l08puw2.zip"))
sipp <- data.frame(matrix(NA,nrow=dim(sipp_raw)[1],ncol=dim(input)[1]))
colnames(sipp) <- rownames(input)
	
counter <- 0
for(i in colnames(sipp)) { 
	sipp[,i] <- as.numeric(substr(as.character(sipp_raw[,1]),start=counter+1,stop=counter+input[i,"width"]))
	counter <- counter+input[i,"width"]
}
rm(sipp_raw)
gc()	

sipp <- sipp[sipp$SREFMON == 4,] # Use the last reference month (there are four months per person in the SIPP)
rownames(sipp) <- paste(sipp$SSUID,sipp$EPPPNUM,sep="")
sipp[,colnames(sipp_tm2)] <- sipp_tm2[rownames(sipp),]
to.save <- c()

	
# Create Unauthorized Variables

sipp$unauthorized <- 1

	# Can remove citizens
	sipp <- sipp[sipp$ECITIZNT == 2,]
	acs <- acs[acs$CITIZEN == 3,]
	
	# Permanent residents are authorized
	sipp[sipp$EADJUST == 1 | sipp$TIMSTAT == 1,"unauthorized"] <- 0
	
	# Federal program participation 
	sipp[sipp$ESSSELF == 1 | sipp$ESSISELF == 1 | sipp$EWICYN == 1 | sipp$EASST01 == 1 |
			sipp$RCUTYP08 == 1 | sipp$RCUTYP20 == 1 | sipp$RCUTYP27 == 1,"unauthorized"] <- 0 
	
	# Take out college students
	sipp[sipp$EENLEVEL %in% c(3:8),"unauthorized"] <- 0
	
# Now we need to prepare the covariates

	# Duration of US residence
	sipp$duration_in_US <- NA
	sipp[sipp$TMOVEUS == 22,"duration_in_US"] <- 1
	sipp[sipp$TMOVEUS == 21,"duration_in_US"] <- 2
	sipp[sipp$TMOVEUS == 20,"duration_in_US"] <- 3
	sipp[sipp$TMOVEUS == 19,"duration_in_US"] <- 4
	sipp[sipp$TMOVEUS == 18,"duration_in_US"] <- 5
	sipp[sipp$TMOVEUS == 17,"duration_in_US"] <- 6.5
	sipp[sipp$TMOVEUS == 16,"duration_in_US"] <- 8
	sipp[sipp$TMOVEUS == 15,"duration_in_US"] <- 9
	sipp[sipp$TMOVEUS == 14,"duration_in_US"] <- 10
	sipp[sipp$TMOVEUS == 13,"duration_in_US"] <- 11.5
	sipp[sipp$TMOVEUS == 12,"duration_in_US"] <- 13.5
	sipp[sipp$TMOVEUS == 11,"duration_in_US"] <- 15.5
	sipp[sipp$TMOVEUS == 10,"duration_in_US"] <- 17.5
	sipp[sipp$TMOVEUS == 9,"duration_in_US"] <- 19.5
	sipp[sipp$TMOVEUS == 8,"duration_in_US"] <- 22
	sipp[sipp$TMOVEUS == 7,"duration_in_US"] <- 24.5
	sipp[sipp$TMOVEUS == 6,"duration_in_US"] <- 27
	sipp[sipp$TMOVEUS == 5,"duration_in_US"] <- 29.5
	sipp[sipp$TMOVEUS == 4,"duration_in_US"] <- 33
	sipp[sipp$TMOVEUS == 3,"duration_in_US"] <- 37.5
	sipp[sipp$TMOVEUS == 2,"duration_in_US"] <- 44.5
	sipp[sipp$TMOVEUS == 1,"duration_in_US"] <- 49

	acs$duration_in_US <- acs$YEAR - acs$YRIMMIG
	
	# Place of birth
		# SIPP: North America, South America, Central America, Caribbean, North/West Europe, South/East Europe	
				# Eastern Asia, South Central Asia, South East Asia/West Asia/Australia/New Zealand, Africa
	
	sipp$birth_place <- NA
	sipp[sipp$TBRSTATE == 562,"birth_place"] <- "North America"
	sipp[sipp$TBRSTATE == 563,"birth_place"] <- "Northwest Europe"
	sipp[sipp$TBRSTATE == 564,"birth_place"] <- "Southeast Europe"
	sipp[sipp$TBRSTATE == 565,"birth_place"] <- "East Asia"
	sipp[sipp$TBRSTATE == 566,"birth_place"] <- "West_SE Asia, Australia, New Zealand"
	sipp[sipp$TBRSTATE == 567,"birth_place"] <- "West_SE Asia, Australia, New Zealand"
	sipp[sipp$TBRSTATE == 568,"birth_place"] <- "Africa"
	sipp[sipp$TBRSTATE == 569,"birth_place"] <- "Caribbean"
	sipp[sipp$TBRSTATE == 570,"birth_place"] <- "Central America"
	sipp[sipp$TBRSTATE == 571,"birth_place"] <- "South America"
	
	acs$birth_place <- NA
	acs[acs$BPL %in% c(100),"birth_place"] <- "West_SE Asia, Australia, New Zealand"
	acs[acs$BPL %in% c(150,160,200),"birth_place"] <- "North America"
	acs[acs$BPL %in% 210,"birth_place"] <- "Central America"
	acs[acs$BPL %in% c(250,260),"birth_place"] <- "Caribbean"
	acs[acs$BPL %in% c(299,300),"birth_place"] <- "South America"	
	acs[(acs$BPL >= 400 & acs$BPL < 430) | (acs$BPL %in% c(450,452,453,454,455)),"birth_place"] <- "Northwest Europe"
	acs[(acs$BPL >= 430 & acs$BPL <= 441) | acs$BPL %in% c(451,456,457,458) | (acs$BPL >= 459 & acs$BPL < 500) ,"birth_place"]  <- "Southeast Europe"
	acs[acs$BPL %in% c(500,501,502,509),"birth_place"] <- "East Asia"
	acs[acs$BPL >= 510 & acs$BPL < 999 & acs$BPL != 600,"birth_place"] <- "West_SE Asia, Australia, New Zealand"
	acs[acs$BPL == 600,"birth_place"] <- "Africa" 
	
	# Speaks English
	
	acs$English <- 1
	acs[acs$SPEAKENG %in% c(0,1,6),"English"] <- 0
	sipp$English <- 1
	sipp[sipp$EHOWWELL %in% c(3,4),"English"] <- 0

	# FPL - TFTOTINC/THTOTINC/RHPOV, POVERTY
	sipp$FPL <- pmax(0,pmin(sipp$THTOTINC/sipp$RHPOV,5))
	sipp$FPLSQ <- (sipp$FPL)^2
	acs$FPL <- acs$POVERTY/100	
	acs$FPLSQ <- (acs$FPL)^2
	
	# Ethnicity
	
	sipp$Hispanic <- 0
	sipp[sipp$EORIGIN == 1,"Hispanic"] <- 10
	
	acs$Hispanic <- 1
	acs[acs$HISPAN == 0,"Hispanic"] <- 0
	
	# Employment - EPDJBTHN/EJOBCNTR, EMPSTAT
	sipp$employed <- 0
	sipp[sipp$EPDJBTHN == 1,"employed"] <- 1
	acs$employed <- 0
	acs[acs$EMPSTAT == 1,"employed"] <- 1
	
	# AGE
	sipp$AGE <- sipp$TAGE
	sipp$TAGE <- NULL
	
	sipp$AGESQ <- (sipp$AGE)^2
	acs$AGESQ <- (acs$AGE)^2
	
	# Gender
	sipp$SEX <- sipp$ESEX
	sipp[sipp$SEX == 2,"SEX"] <- 0
	acs[acs$SEX == 2,"SEX"] <- 0
	sipp$ESEX <- NULL
	
	# Race
	sipp$Race <- NA
	sipp[sipp$ERACE == 1,"Race"] <- "White"
	sipp[sipp$ERACE == 2,"Race"] <- "Black"
	sipp[sipp$ERACE == 3,"Race"] <- "Asian"
	sipp[sipp$ERACE == 4,"Race"] <- "Other"
	
	acs$Race <- NA
	acs[acs$RACE == 1,"Race"] <- "White"
	acs[acs$RACE == 2,"Race"] <- "Black"
	acs[acs$RACE %in% c(4,5,6),"Race"] <- "Asian"
	acs[acs$RACE %in% c(3,7,8,9),"Race"] <- "Other"
	
	# Industry - EJBIND1, IND
	sipp$industry <- "not employed"
	sipp[sipp$EJBIND1 < 1000,"industry"] <- "agriculture"
	sipp[sipp$EJBIND1 >= 1000 & sipp$EJBIND1 < 4000,"industry"] <- "manufacturing"
	sipp[sipp$EJBIND1 >= 4000 & sipp$EJBIND1 < 9000,"industry"] <- "services"
	sipp[sipp$EJBIND1 >= 9000,"industry"] <- "other"  
	
	acs$industry <- "not employed"
	acs[acs$IND < 1000,"industry"] <- "agriculture"
	acs[acs$IND >= 1000 & acs$IND < 4000,"industry"] <- "manufacturing"
	acs[acs$IND >= 4000 & acs$IND < 9000,"industry"] <- "services"
	acs[acs$IND >= 9000,"industry"] <- "other"  
	
	# Marital Status
	sipp$married <- 0
	sipp[sipp$EMS %in% c(1,2),"married"] <- 1
	acs$married <- 0
	acs[acs$MARST %in% c(1,2),"married"] <- 1
	
	# Parental Status - ERRP, RELATE
	
	# Educational attainment - EEDUCATE, EDUC
	sipp$education_level <- NA
	sipp[sipp$EEDUCATE < 39,"education_level"] <- "lt_high_school"
	sipp[sipp$EEDUCATE == 39,"education_level"] <- "high_school"
	sipp[sipp$EEDUCATE > 39,"education_level"] <- "gt_high_school"
	
	acs$education_level <- NA
	acs[acs$EDUC < 6,"education_level"] <- "lt_high_school"
	acs[acs$EDUC == 6,"education_level"] <- "high_school"
	acs[acs$EDUC > 6,"education_level"] <- "gt_high_school"
	
	# Family size - EHHNUMPP/EFNP, FAMSIZE
	sipp$FAMSIZE <- sipp$EFNP
	sipp$EFNP <- NULL
	
	# Insurance Coverage - EHIMTH/ECRMTH/ECDMTH, HCOVANY
	sipp$uninsured <- 0
	sipp[sipp$EHIMTH == 2 & sipp$ECRMTH == 2 & sipp$ECDMTH == 2,"uninsured"] <- 1	
	acs$uninsured <- 0
	acs[acs$HCOVANY == 1,"uninsured"] <- 1
	
	# Homeownership - ETENURE, OWNERSHIP
	sipp$home_owner <- 0
	sipp[sipp$ETENURE == 1,"home_owner"] <- 1
	acs$home_owner <- 0
	acs[acs$OWNERSHP == 1,"home_owner"] <- 1
	
##### Impute Unauthorized Immigrants
	# Estimate logit to predict unauthorized immigrants in the population of non-citizens
	# Estimate logit to predict permanent residents in the population of authorized non-citizens
	# http://www.dhs.gov/sites/default/files/publications/ois_ill_pe_2012_2.pdf
	
	# Estimation in SIPP
	
	#spec <- unauthorized ~ duration_in_US + birth_place + English + FPL + employed + AGE + AGESQ + SEX + Hispanic + 
	#	Race + as.factor(industry) + married + as.factor(education_level) + FAMSIZE + uninsured + home_owner
	spec <- unauthorized ~ duration_in_US + birth_place + English + FPL + employed + AGE + SEX + Hispanic + 
		Race + as.factor(industry) + married + as.factor(education_level) + FAMSIZE + uninsured + home_owner
	
	unauthorized_logit <- glm(spec, data = sipp, family=binomial(link='logit'))
	summary(unauthorized_logit)

	# Predict in ACS
		# In 2012, 2.82 million undocumented in CA, 0.27 million in WA, 0.58 million in NY (DHS data)
		# According to ACS: There are 5.3 million noncitizens in CA, of which 1.9 million are uninsured
		
	set.seed(5)
	unique_years <- length(unique(acs$YEAR))
	unauthorized_predictions <- predict.glm(unauthorized_logit,newdata=acs,type='response')
	size <- unique_years * round(2820000/(sum(acs$PERWT)/100)*dim(acs)[1]) # This is rough, but precision isn't important
	unauthorized_immigrants <- sample(rownames(acs),size=size,replace=FALSE,prob=unauthorized_predictions) 
	
	acs$undocumented_immigrant <- 0
	acs[unauthorized_immigrants,"undocumented_immigrant"] <- 1
	
##### Impute Permanent Residents
	# Estimate logit to predict permanent residents in the population of authorized non-citizens
	# 3.4 million in CA in 2012 https://www.dhs.gov/sites/default/files/publications/ois_lpr_pe_2012.pdf
	
	# Estimation in SIPP
	
	sipp_authorized <- subset(sipp,unauthorized == 0)
	sipp_authorized$permanent_resident <- 0
	sipp_authorized[sipp_authorized$EADJUST == 1 | sipp_authorized$TIMSTAT == 1,"permanent_resident"] <- 1

	spec <- permanent_resident ~ duration_in_US + birth_place + English + FPL + employed + AGE + SEX + Hispanic + 
		Race + as.factor(industry) + married + as.factor(education_level) + FAMSIZE + uninsured + home_owner
	unauthorized_logit <- glm(spec, data = sipp_authorized, family=binomial(link='logit'))
	summary(unauthorized_logit)

	# Predict in ACS
	
	acs_authorized <- subset(acs,undocumented_immigrant == 0)
	permanent_predictions <- predict.glm(unauthorized_logit,newdata=acs_authorized,type='response')
	size <- round(dim(sipp_authorized[sipp_authorized$permanent_resident,])[1]/dim(sipp_authorized)[1] * dim(acs_authorized)[1]) # This is rough, but precision isn't important
	permanent_residents <- sample(rownames(acs_authorized),size=size,replace=FALSE,prob=permanent_predictions) 
	
	acs$permanent_resident <- 0
	acs[permanent_residents,"permanent_resident"] <- 1

	acs$temporary_resident <- 0
	acs[acs$permanent_resident == 0 & acs$undocumented_immigrant == 0,"temporary_resident"] <- 1
	
##### Save new ACS fields

to.save <- c("undocumented_immigrant","permanent_resident","temporary_resident","duration_in_US")
acs_to_save <- acs[,to.save]
rownames(acs_to_save) <- paste(acs$HIUID,acs$PERNUM,acs$AGE,acs$YEAR,sep="_")
save(acs_to_save,file="data/final/acs_immigration")	



############################################################################################################

###############################
### Impute Affordable Offer ###
###############################

# Objective: impute in ACS whether an uninsured person has an affordable employer offer

# Variables in SIPP
	# TAMTPLAN: amount paid for health plan
	# EHEALPLA: does employer offer coverage to any of its employees
	# ENOTPLAN: reason not covered by employer offer (ineligible, denied coverage, chose not be covered, other)
	# ECOVMEMB: can family members be covered under plan?

##### Unzip IPUMS file
input <- parse.SAScii("data/ACS-SIPP/input2014.sas")
rownames(input) <- input$varname
ipums_data <- read.table(gzfile("data/ACS-SIPP/ipumsCA.gz"),skipNul = TRUE)

acs <- data.frame(matrix(NA,nrow=dim(ipums_data)[1],ncol=dim(input)[1]))
colnames(acs ) <- rownames(input)

counter <- 0
for(i in colnames(acs )) { 
	acs[,i] <- as.numeric(substr(as.character(ipums_data[,1]),start=counter+1,stop=counter+input[i,"width"]))
	counter <- counter+input[i,"width"]
}
rm(ipums_data)
gc()

##### Unzip SIPP topical module 6 data
input <- parse.SAScii("data/ACS-SIPP/inputSIPP_tm6.sas")
rownames(input) <- input$varname
sipp_raw <- read.table(unzip("data/ACS-SIPP/p08putm6.zip")) 
sipp_tm6 <- data.frame(matrix(NA,nrow=dim(sipp_raw)[1],ncol=dim(input)[1]))
colnames(sipp_tm6) <- rownames(input)
	
counter <- 0
for(i in colnames(sipp_tm6)) { 
	sipp_tm6[,i] <- as.numeric(substr(as.character(sipp_raw[,1]),start=counter+1,stop=counter+input[i,"width"]))
	counter <- counter+input[i,"width"]
}
rm(sipp_raw)
gc()

rownames(sipp_tm6) <- paste(sipp_tm6$SSUID,sipp_tm6$EPPPNUM,sep="")
to.save <- c("TAMTPLAN","EHEALPLA","ENOTPLAN","ECOVMEMB","ENOELIG1","ENOELIG2","ENOELIG3")
sipp_tm6 <- sipp_tm6[,to.save]	


##### Unzip SIPP core wave 6 data
input <- parse.SAScii("data/ACS-SIPP/inputSIPP_core.sas")
rownames(input) <- input$varname
sipp_raw <- read.table(unzip("data/ACS-SIPP/l08puw6.zip"))
sipp <- data.frame(matrix(NA,nrow=dim(sipp_raw)[1],ncol=dim(input)[1]))
colnames(sipp) <- rownames(input)

	
counter <- 0
for(i in colnames(sipp)) { 
	sipp[,i] <- as.numeric(substr(as.character(sipp_raw[,1]),start=counter+1,stop=counter+input[i,"width"]))
	counter <- counter+input[i,"width"]
}
rm(sipp_raw)
gc()	

sipp <- sipp[sipp$SREFMON == 4,] # Use the last reference month (there are four months per person in the SIPP)
rownames(sipp) <- paste(sipp$SSUID,sipp$EPPPNUM,sep="")
sipp[,colnames(sipp_tm6)] <- sipp_tm6[rownames(sipp),]
gc()

##### Prepare Variables

	# Insurance Coverage - EHIMTH/ECRMTH/ECDMTH, HCOVANY
		# We want to impute whether an "uninsured household" (i.e., no one on ESI at least one uninsured) has an offer
		# Naturally, the household must have at least one employed person
		
	sipp$uninsured <- 0
	sipp[sipp$EHIMTH == 2 & sipp$ECRMTH == 2 & sipp$ECDMTH == 2,"uninsured"] <- 1	
	sipp$ESI <- as.numeric(sipp$EHEMPLY %in% c(1,2,3))
	sipp$worker <- as.numeric(sipp$EPDJBTHN == 1)
	
	acs$uninsured <- 0
	acs[acs$HCOVANY == 1,"uninsured"] <- 1
	acs$ESI <- as.numeric(acs$HINSEMP == 2)
	acs$worker <- as.numeric(acs$EMPSTAT == 1)
	
	sipp_households <- as.data.frame(cbind(by(sipp$ESI,sipp$SSUID,sum),by(sipp$uninsured,sipp$SSUID,sum),by(sipp$worker,sipp$SSUID,sum)))
	colnames(sipp_households) <- c("number_ESI","number_uninsured","number_workers")
	acs_households <- as.data.frame(cbind(by(acs$ESI,acs$HIUID,sum),by(acs$uninsured,acs$HIUID,sum),by(acs$worker,acs$HIUID,sum)))
	colnames(acs_households) <- c("number_ESI","number_uninsured","number_workers")
	
		# Subset to households meeting above criteria
		sipp_households$impute <- as.numeric(sipp_households$number_ESI == 0 & 
												sipp_households$number_uninsured > 0 & 
												sipp_households$number_workers > 0)
		acs_households$impute <- as.numeric(acs_households$number_ESI == 0 & 
												acs_households$number_uninsured > 0 & 
												acs_households$number_workers > 0)
												
		sipp <- sipp[sipp$SSUID %in% rownames(sipp_households[sipp_households$impute == 1,]),]
		acs <- acs[acs$HIUID %in% rownames(acs_households[acs_households$impute == 1,]),]
	
	# Employer offer
		# EHEALPLA: does employer offer coverage to any of its employees
		# ENOTPLAN: reason not covered by employer offer (ineligible, denied coverage, chose not be covered, other)
		# ECOVMEMB: can family members be covered under plan?
	
	sipp$employer_offer <- NA
	sipp[sipp$EHEALPLA == 1 & sipp$ENOTPLAN %in% c(3),"employer_offer"] <- 1
	sipp[sipp$EHEALPLA == 2 | sipp$ENOTPLAN %in% c(1,2,4),"employer_offer"] <- 0
	
	# FPL - TFTOTINC/THTOTINC/RHPOV, POVERTY
	sipp$FPL <- pmax(0,pmin(sipp$THTOTINC/sipp$RHPOV,5))
	acs$FPL <- acs$POVERTY/100	
	
	# Ethnicity
	
	sipp$Hispanic <- 0
	sipp[sipp$EORIGIN == 1,"Hispanic"] <- 10
	
	acs$Hispanic <- 1
	acs[acs$HISPAN == 0,"Hispanic"] <- 0
	
	# AGE
	sipp$AGE <- sipp$TAGE
	sipp$TAGE <- NULL
	
	# Gender
	sipp$SEX <- sipp$ESEX
	sipp[sipp$SEX == 2,"SEX"] <- 0
	acs[acs$SEX == 2,"SEX"] <- 0
	sipp$ESEX <- NULL
	
	# Race
	sipp$Race <- NA
	sipp[sipp$ERACE == 1,"Race"] <- "White"
	sipp[sipp$ERACE == 2,"Race"] <- "Black"
	sipp[sipp$ERACE == 3,"Race"] <- "Asian"
	sipp[sipp$ERACE == 4,"Race"] <- "Other"
	
	acs$Race <- NA
	acs[acs$RACE == 1,"Race"] <- "White"
	acs[acs$RACE == 2,"Race"] <- "Black"
	acs[acs$RACE %in% c(4,5,6),"Race"] <- "Asian"
	acs[acs$RACE %in% c(3,7,8,9),"Race"] <- "Other"
	
	# Industry - EJBIND1, IND
	sipp$industry <- "other"
	sipp[sipp$EJBIND1 <= 290,"industry"] <- "agriculture"
	sipp[(sipp$EJBIND1 >= 370 & sipp$EJBIND1 < 500) | (sipp$EJBIND1 >= 700 & sipp$EJBIND1 < 1000),"industry"] <- "mining_construction"
	sipp[sipp$EJBIND1 >= 1000 & sipp$EJBIND1 < 4000,"industry"] <- "manufacturing"
	sipp[sipp$EJBIND1 >= 4000 & sipp$EJBIND1 < 6000,"industry"] <- "retail_wholesale"
	sipp[(sipp$EJBIND1 >= 6000 & sipp$EJBIND1 < 6400) | (sipp$EJBIND1 >= 500 & sipp$EJBIND1 < 700),"industry"] <- "transportation_utilities"
	sipp[sipp$EJBIND1 >= 6800 & sipp$EJBIND1 < 7200,"industry"] <- "finance"
	sipp[sipp$EJBIND1 >= 7200 & sipp$EJBIND1 < 8500,"industry"] <- "services"
	sipp[sipp$EJBIND1 >= 8500 & sipp$EJBIND1 < 8700,"industry"] <- "leisure"
	#sipp[(sipp$EJBIND1 >= 6400 & sipp$EJBIND1 < 6800) | (sipp$EJBIND1 >= 8700 & sipp$EJBIND1 < 9300) ,"industry"] <- "other"  
	sipp[sipp$EJBIND1 >= 9300,"industry"] <- "government"  
	
	acs$industry <- "other"
	acs[acs$IND <= 290,"industry"] <- "agriculture"
	acs[(acs$IND >= 370 & acs$IND < 500) | (acs$IND >= 700 & acs$IND < 1000),"industry"] <- "mining_construction"
	acs[acs$IND >= 1000 & acs$IND < 4000,"industry"] <- "manufacturing"
	acs[acs$IND >= 4000 & acs$IND < 6000,"industry"] <- "retail_wholesale"
	acs[(acs$IND >= 6000 & acs$IND < 6400) | (acs$IND >= 500 & acs$IND < 700),"industry"] <- "transportation_utilities"
	acs[acs$IND >= 6800 & acs$IND < 7200,"industry"] <- "finance"
	acs[acs$IND >= 7200 & acs$IND < 8500,"industry"] <- "services"
	acs[acs$IND >= 8500 & acs$IND < 8700,"industry"] <- "leisure"
	#acs[(acs$IND >= 6400 & acs$IND < 6800) | (acs$IND >= 8700 & acs$IND < 9300) ,"industry"] <- "other"  
	acs[acs$IND >= 9300,"industry"] <- "government"  
	
	# Educational attainment - EEDUCATE, EDUC
	sipp$education_level <- NA
	sipp[sipp$EEDUCATE < 39,"education_level"] <- "lt_high_school"
	sipp[sipp$EEDUCATE == 39,"education_level"] <- "high_school"
	sipp[sipp$EEDUCATE > 39,"education_level"] <- "gt_high_school"
	
	acs$education_level <- NA
	acs[acs$EDUC < 6,"education_level"] <- "lt_high_school"
	acs[acs$EDUC == 6,"education_level"] <- "high_school"
	acs[acs$EDUC > 6,"education_level"] <- "gt_high_school"
	
	# Family size - EHHNUMPP/EFNP, FAMSIZE
	sipp$FAMSIZE <- sipp$EFNP
	sipp$EFNP <- NULL
	
	# Hours Worked - EHRSALL, UHRSWORK
	sipp$hours_worked <- NA
	sipp[sipp$EHRSALL > 0,"hours_worked"] <- sipp[sipp$EHRSALL > 0,"EHRSALL"]
	acs$hours_worked <- NA
	acs[acs$UHRSWORK > 0,"hours_worked"] <- acs[acs$UHRSWORK > 0,"UHRSWORK"]
	
##### Predict Employer Offer
	# Covariates
		# income to poverty ratio - TFTOTINC/THTOTINC/RHPOV, POVERTY
		# Ethnicity - EORIGIN, HISPAN
		# Age - TAGE, AGE
		# Gender - ESEX, SEX
		# Race - ERACE, RACE
		# Industry - EJBIND1, IND
		# Educational attainment - EEDUCATE, EDUC
		# Household size - EHHNUMPP/EFNP, FAMSIZE
		# Hours worked per week - EHRSALL, UHRSWORK
		# Occupation? 
		# Temporary workers
		# Contract workers
	
	sipp_workers <- sipp[!is.na(sipp$employer_offer),]
	acs_workers <- acs[acs$worker == 1,]
	
	spec <- employer_offer ~ FPL + Hispanic + AGE + SEX + Race + industry + education_level + FAMSIZE + hours_worked
	offer_logit <- glm(spec, data = sipp_workers, family=binomial(link='logit'))
	summary(offer_logit)

	# Predict in ACS
		
	set.seed(5)
	offer_predictions <- predict.glm(offer_logit,newdata=acs_workers,type='response')
	acs_workers$employer_offer <- as.numeric(offer_predictions - runif(offer_predictions) > 0)
	employer_offers <- by(acs_workers$employer_offer,acs_workers$HIUID,sum)
	
	acs$employer_offer <- NA
	acs[rownames(acs_workers),"employer_offer"] <- acs_workers$employer_offer
	acs$access_to_emp_offer <- as.numeric(employer_offers[as.character(acs$HIUID)] > 0)
		
	##### Save new ACS fields

	to.save <- c("employer_offer","access_to_emp_offer")
	acs_to_save <- acs[,to.save]
	rownames(acs_to_save) <- paste(acs$HIUID,acs$PERNUM,acs$AGE,acs$YEAR,sep="_")
	save(acs_to_save,file="data/final/acs_emp_offer")	

	
	