# Load Data

	# Rate filing data
		
		#### NOTE: I need to ask Sam how he constructed the merged file
	
	load("data/2014-2020.RData")
	library(plyr)
	library(dplyr)

	year_to_run <- 2018

	data2014 <- data.frame(PUF_2014)
	rm(PUF_2014)

	data2015 <- data.frame(PUF_2015)
	rm(PUF_2015)

	data2016 <- data.frame(PUF_2016)
	rm(PUF_2016)

	data2017 <- data.frame(PUF_2017)
	rm(PUF_2017)

	data2018 <- data.frame(PUF_2018)
	rm(PUF_2018)

	data2019 <- data.frame(PUF_2019)
	rm(PUF_2019)

	data2020 <- data.frame(PUF_2020)
	rm(PUF_2020)

	gc()
	
	# MLR Data
	mlr_data <- read.csv("data/final/mlr_data.csv",header=TRUE)
	rownames(mlr_data) <- paste(mlr_data$HIOS_ISSUER_ID,mlr_data$year,sep="_")
	
#### Preliminary Data Cleaning	
	
	# Drop Small Group and NON-CA Plans

	data2014 <- data2014[!data2014$MARKET == "Small Group" & data2014$STATE %in% c("CA"),]
	data2015 <- data2015[!data2015$MARKET == "Small Group" & data2015$STATE %in% c("CA"),]
	data2016 <- data2016[!data2016$MARKET == "Small Group" & data2016$STATE %in% c("CA"),]
	data2017 <- data2017[!data2017$MARKET == "Small Group" & data2017$STATE %in% c("CA"),]
	data2018 <- data2018[!data2018$MARKET == "Small Group" & data2018$STATE %in% c("CA"),]
	data2019 <- data2019[!data2019$MARKET == "Small Group" & data2019$STATE %in% c("CA"),]
	data2020 <- data2020[!data2020$MARKET == "Small Group" & data2020$STATE %in% c("CA"),]
	
	# Some missing HIOS
	data2014[data2014$COMPANY == "California Physician's Service, dba Blue Shield of California","ISSUER_ID"] <- 70285
	data2014[data2014$COMPANY == "Chinese Community Health Plan","ISSUER_ID"] <- 47579
	data2014[data2014$COMPANY == "Local Initiative Health Authority for Los Angeles County, dba L.A. Care Health Plan","ISSUER_ID"] <- 92815
	data2014[data2014$COMPANY == "Molina Healthcare of California","ISSUER_ID"] <- 18126
	data2014[data2014$COMPANY == "Sharp Health Plan","ISSUER_ID"] <- 92499
	data2014[data2014$COMPANY == "County of Santa Clara dba Valley Health Plan","ISSUER_ID"] <- 84014
	data2014[data2014$COMPANY == "Western Health Advantage","ISSUER_ID"] <- 93689
	data2015[data2015$COMPANY == "Sharp Health Plan","ISSUER_ID"] <- 92499
	data2016[is.na(data2016$ISSUER_ID),"ISSUER_ID"] <- 92499
	data2017[is.na(data2017$ISSUER_ID),"ISSUER_ID"] <- 92499

	# Some missing plan IDs - from URRTs
	data2014[data2014$ISSUER_ID == 70285,"PLAN_ID"] <- 
		c("70285CA1250001","70285CA1270001","70285CA1290001","70285CA1310001","70285CA1350001",
			"70285CAMissingG","70285CAMissingSIL","70285CAMissingBR","70285CAMissingSIL2","70285CA1320001")
	data2014[data2014$ISSUER_ID == 47579,"PLAN_ID"] <- 
		c("47579CA0180001","47579CA0200001","47579CA0220001","47579CA0240001","47579CA0260001")
	data2014[data2014$ISSUER_ID == 92815,"PLAN_ID"] <- 
		c("92815CA001005","92815CA001004","92815CA001008","92815CA001007","92815CA001006")
	data2014[data2014$ISSUER_ID == 18126,"PLAN_ID"] <- 
		c("18126CA0010001","18126CA0010002","18126CA0010003","18126CA0010004","18126CA0010005")
	data2014[data2014$ISSUER_ID == 84014,"PLAN_ID"] <- 
		c("84014CA0010001","84014CA0010002","84014CA0010003","84014CA0010004","84014CA0010005")
	data2014[data2014$ISSUER_ID == 93689,"PLAN_ID"] <- 
		c("93689CA0110001","93689CA0110002","93689CA0120001","93689CA0120004","93689CA0120005","93689CA0130002")	
	data2014[data2014$ISSUER_ID == 92499,"PLAN_ID"] <-
		c("92499CA0020001","92499CA0020002","92499CA0020003","92499CA0020004","92499CA0020005",
			"92499CA0020006","92499CA0020007","92499CA0020008","92499CA0020009","92499CA0020010")
	data2015[data2015$ISSUER_ID == 92499,"PLAN_ID"] <-
		c("92499CA0020001","92499CA0020002","92499CA0020003","92499CA0020004","92499CA0020005",
			"92499CA0020006","92499CA0020008","92499CA0020009","92499CA0020010")
	data2016[data2016$ISSUER_ID == 92499,"PLAN_ID"] <-
		c("92499CA0020001","92499CA0020002","92499CA0020003","92499CA0020004","92499CA0020005",
			"92499CA0020006","92499CA0020008","92499CA0020009","92499CA0020010")
	data2017[data2017$ISSUER_ID == 92499,"PLAN_ID"] <-
		c("92499CA0020001","92499CA0020002","92499CA0020003","92499CA0020004","92499CA0020005",
			"92499CA0020006","92499CA0020008","92499CA0020009","92499CA0020010")
	
	# Missing Data:
		# I discovered that there were some (10) key missing plans for Health Net PPO in the 2014 rate filing
		# Because the data from URRT 2014 are in PDF form, I am going to enter this manually here
		# The existing 2014 Health Net PPO plans are bogus and should be deleted
		# There are a lot of bogus plans for 2015 too
		
	data2014 <- data2014[!data2014$ISSUER_ID == 99110,]	
	data2015 <- data2015[!(data2015$ISSUER_ID == 99110 & data2015$PRJ_MM == 0),]
	
	data2014_to_add <- data.frame(matrix(NA,10,ncol(data2014)))
	colnames(data2014_to_add) <- colnames(data2014)
	
	data2014_to_add[,"METAL"] <- c("Platinum","Gold","Silver","Bronze","Catastrophic",
									"Platinum","Gold","Silver","Bronze","Catastrophic")
	data2014_to_add[,"PLAN_ID"] <- c("99110CA0210007","99110CA0210008","99110CA0210009","99110CA0210010","99110CA0210011",
									"99110CA0290001","99110CA0290002","99110CA0290003","99110CA0290004","99110CA0290005")
	data2014_to_add[,"COMPANY"] <- "Health_Net_PPO"
	data2014_to_add[,"ISSUER_ID"] <- 99110
	data2014_to_add[,"EXCHANGE"] <- c(rep("No",5),rep("Yes",5))
	data2014_to_add[,"PLAN_TYPE"] <- "PPO"
	data2014_to_add[,"PRJ_MM"] <- c(6905,13810,46654,67373,54988,22971,45943,155209,224138,182936)
	data2014_to_add[,"PRJ_INC_CLM"] <- c(2192632,3853379,11329828,14133507,11535382,7294272,12819392,37692186,47019666,38376311)
	data2014_to_add[,"PRJ_REIN"] <- c(313681,627362,2119403,3060629,2498001,1043529,2087104,7050853,10182168,8310438)
	data2014_to_add[,"PRJ_RSK_ADJ"] <- c(-139695,-279391,-943860,-1363027,-1112466,-464728,-929476,-3140043,-4534549,-3700989)

	data2014 <- rbind(data2014,data2014_to_add)
	
	# Combine data
		
		# NOTE: data2014 and data2015 only have projections for 2014 and 2015, respectively
			# don't use for experience
		
		data2014$year <- 2012
		data2015$year <- 2013
		data2016$year <- 2014
		data2017$year <- 2015
		data2018$year <- 2016
		data2019$year <- 2017
		data2020$year <- 2018

		# Straightforward to combine 2014-2017
		data2014$EXP_PRM_PMPM <- NULL
		data2015$EXP_PRM_PMPM <- NULL
		data2014$EXP_PLN_ADJ_INDX <- NA
		data2015$EXP_PLN_ADJ_INDX <- NA
		data2014$PLAN_CAT <- NA
		data2015$PLAN_CAT <- NA
		data2016$PLAN_CAT <- NA
		rdata <- rbind(data2014[,colnames(data2017)],data2015[,colnames(data2017)],
			data2016[,colnames(data2017)],data2017,data2018,data2019)
		
		# Now merge in 2020
		data2020$EXP_REIN <- data2020$EXP_REINS
		data2020$PRJ_REIN <- data2020$PRJ_REINS
		data2020[,setdiff(colnames(rdata),colnames(data2020))] <- NA
		rdata <- rbind(rdata,data2020[,colnames(rdata)])
	
	# Clean up company names
	rdata$COMPANY <- as.character(rdata$COMPANY)
	rdata[rdata$COMPANY %in% c("Anthem Blue Cross Life and Health Insurance Company (CDI)","Anthem Blue Cross (DMHC)"),"COMPANY"] <- "Anthem"
	rdata[rdata$COMPANY == "California Physician's Servce, dba Blue Shield of California","COMPANY"] <- "Blue_Shield"
	rdata[rdata$COMPANY == "California Physician's Service, dba Blue Shield of California","COMPANY"] <- "Blue_Shield"
	rdata[rdata$COMPANY == "Kaiser Foundation Health Plan, Inc.","COMPANY"] <- "Kaiser"
	rdata[rdata$ISSUER_ID == 67138,"COMPANY"] <- "Health_Net_HMO"
	rdata[rdata$ISSUER_ID == 99110,"COMPANY"] <- "Health_Net_PPO"
	rdata[rdata$COMPANY == "Chinese Community Health Plan","COMPANY"] <- "Chinese_Community"
	rdata[rdata$COMPANY == "Local Initiative Health Authority for Los Angeles County, dba L.A. Care Health Plan","COMPANY"] <- "LA_Care"
	rdata[rdata$COMPANY %in% c("Molina Healthcare of California","Molina Healthcare of California, Inc."),"COMPANY"] <- "Molina"
	rdata[rdata$COMPANY == "County of Santa Clara dba Valley Health Plan","COMPANY"] <- "Valley"
	rdata[rdata$COMPANY == "Sharp Health Plan","COMPANY"] <- "Sharp"
	rdata[rdata$COMPANY == "Western Health Advantage","COMPANY"] <- "Western"
	rdata[rdata$COMPANY == "Oscar Health Plan of California","COMPANY"] <- "Oscar"
	rdata[rdata$COMPANY == "CONTRA COSTA MEDICAL SERVICES DBA CONTRA COSTA HEALTH PLAN","COMPANY"] <- "Contra_Costa"
	rdata[rdata$COMPANY == "UnitedHealthcare Benefits Plan of California","COMPANY"] <- "United"
	rdata[rdata$COMPANY == "Cigna Health and Life Insurance Company","COMPANY"] <- "Cigna"
	rdata[rdata$COMPANY %in% c("Sutter Health Plan","Sutter Health Plus"),"COMPANY"] <- "Sutter"
	
	# Off-Exchange firms not in Risk Adjustment Pool can be dropped
	
	drop_firms <- c("Celtic Insurance Company","Connecticut General Life Insurance Company")
	rdata <- rdata[!rdata$COMPANY %in% drop_firms,]
	
#### Need to align projections with actuals
	# PRJ_PLAN_ID: for a plan in year t, refers to the plan with its projections (which occured in year t-2)
	# EXP_PLAN_ID: for a plan in year t, refers to the plan with its realized experience (which occurred in year t+2)
	# Every plan in years 2012-2016 needs a EXP_PLAN_ID
	# Every plan in years 2014-2018 needs a PRJ_PLAN_ID
	# Eventually, we will drop years 2012 and 2013, but still need the projections
	# need to grab PRJ_MM, PRJ_INC_CLM, PRJ_REIN, PRJ_RSK_ADJ
	
	# Initial match
	
		rdata$PRJ_PLAN_ID <- NA
		rdata$EXP_PLAN_ID <- NA
		rdata$PLAN_ID_YEAR <- paste(rdata$PLAN_ID,rdata$year,sep="_")
		rownames(rdata) <- paste(rdata$PLAN_ID,rdata$year,sep="_")
		
		# Easiest way to match is use PLAN_ID 
		matches <- rdata[paste(rdata$PLAN_ID,rdata$year-2,sep="_") %in% rdata$PLAN_ID_YEAR,"PLAN_ID_YEAR"]
		rdata[matches,"PRJ_PLAN_ID"] <- paste(rdata[matches,"PLAN_ID"],rdata[matches,"year"]-2,sep="_")
		
		matches <- rdata[paste(rdata$PLAN_ID,rdata$year+2,sep="_") %in% rdata$PLAN_ID_YEAR,"PLAN_ID_YEAR"]		
		rdata[matches,"EXP_PLAN_ID"] <- paste(rdata[matches,"PLAN_ID"],rdata[matches,"year"]+2,sep="_")
		
	# Fix 2018
	
		# 24 plans missing PRJ_PLAN_ID
			# 6 Anthem plans not matched - all were new plans for 2020 (zero experience in 2018) so can delete
			# 5 Blue Shield plans not matched - all were new plans for 2020 (zero experience in 2018) so can delete
			# 6 Health Net plans not matched - none were offered in 2018 (zero experience in 2018) so can delete
			# 4 Oscar plans not matched - all were new plans for 2020 (zero experience in 2018) so can delete
			# 1 new SHARP plan - can delete
			# 1 missing Chinese Community - merge w/ another silver off-exchange plan
		
		rdata <- rdata[!(rdata$year == 2018  & is.na(rdata$PRJ_PLAN_ID) & 
			rdata$COMPANY %in% c("Anthem","Blue_Shield","Health_Net_PPO","Oscar","Sharp")),]
		rdata[rdata$year == 2018 & rdata$COMPANY == "Chinese_Community" & is.na(rdata$PRJ_PLAN_ID),"PRJ_PLAN_ID"] <- "47579CA0600001_2016"
	
		# 37 plans missing EXP_PLAN_ID
			# 24 Anthem plans not matched - all have zero projected member months, so you can leave it
			# 2 Blue shield plans not matched - all have zero projected member months, so you can leave it
			# 6 Health Net HMO plans not mtached - all have zero projected member months, so you can leave it
			# 1 Chinese Community plan not matched - merge with another silver plan
			# 4 Oscar plans not matched - all have zero projected member months, so you can leave it
	
		rdata[rdata$year == 2016 & rdata$COMPANY == "Chinese_Community" & is.na(rdata$EXP_PLAN_ID),"EXP_PLAN_ID"] <- "47579CA0020001_2018"
	
	# Fix 2017
	
		# 170 plans missing PRJ_PLAN_ID
			# Anthem: just need to fix IDs
			# Blue Shield: all plans were new and had zero experience, so can toss
			# Health Net: all plans were new and had zero experience, so can toss
			# Western: all plans were new and had zero experience, so can toss
			# Oscar: all plans were new and had zero experience, so can toss
			# Sharp: 1 plan is no and had zero experience, so can toss
			# Chinese Community: can toss 4 plans, reassign 2 of the plans
			
			# Anthem
			rdata["27603CA1500005_2017","PRJ_PLAN_ID"] <- "27603CA1520002_2015"
			rdata["27603CA1500006_2017","PRJ_PLAN_ID"] <- "27603CA1520001_2015"
			rdata["27603CA1500007_2017","PRJ_PLAN_ID"] <- "27603CA1500001_2015"
			rdata["27603CA1500008_2017","PRJ_PLAN_ID"] <- "27603CA1500002_2015"
			rdata["27603CA1500009_2017","PRJ_PLAN_ID"] <- "27603CA1500003_2015"
			rdata["27603CA1500010_2017","PRJ_PLAN_ID"] <- "27603CA1500004_2015"
			rdata["27603CA1510010_2017","PRJ_PLAN_ID"] <- "27603CA1510001_2015"
			rdata["27603CA1510011_2017","PRJ_PLAN_ID"] <- "27603CA1510002_2015"
			rdata["27603CA1510012_2017","PRJ_PLAN_ID"] <- "27603CA1510003_2015"
			rdata["27603CA1510015_2017","PRJ_PLAN_ID"] <- "27603CA1510006_2015"
			
			firms_to_toss <- c("Blue_Shield","Health_Net_HMO","Health_Net_PPO","Western","Oscar","Sharp")
			rdata <- rdata[!(rdata$year == 2017 & rdata$COMPANY %in% firms_to_toss & is.na(rdata$PRJ_PLAN_ID)),]
			
			# Chinese Community
				# Verified we can drop 4 plans with no experience
				# 2 plans remaining
			rdata <- rdata[!(rdata$year == 2017 & rdata$COMPANY == "Chinese_Community" & 
				is.na(rdata$PRJ_PLAN_ID) & rdata$EXP_MM == 0),]
			rdata["47579CA0560001_2017","PRJ_PLAN_ID"] <- "47579CA0110001_2015"
			rdata["47579CA0020001_2017","PRJ_PLAN_ID"] <- "47579CA0120001_2015"
	
		# 53 plans missing EXP_PLAN_ID
			# Anthem: Fix IDs of 23 plans, 1 plan has 0 projected member months so can leave
			# Health Net: all plans have 0 projected member months, so can leave
			# Kaiser: 1 plan has 0 projection member months, so can leave
			# Cigna: wasn't in market in 2017 according to rate filing data, so can leave
			# Chinese Community: reassign IDs

			# Anthem:
			rdata["27603CA1520002_2015","EXP_PLAN_ID"] <- "27603CA1500005_2017"
			rdata["27603CA1520001_2015","EXP_PLAN_ID"] <- "27603CA1500006_2017"
			rdata["27603CA1500001_2015","EXP_PLAN_ID"] <- "27603CA1500007_2017"
			rdata["27603CA1500002_2015","EXP_PLAN_ID"] <- "27603CA1500008_2017"
			rdata["27603CA1500003_2015","EXP_PLAN_ID"] <- "27603CA1500009_2017"
			rdata["27603CA1500004_2015","EXP_PLAN_ID"] <- "27603CA1500010_2017"
			rdata["27603CA1510001_2015","EXP_PLAN_ID"] <- "27603CA1510010_2017"
			rdata["27603CA1510002_2015","EXP_PLAN_ID"] <- "27603CA1510011_2017"
			rdata["27603CA1510003_2015","EXP_PLAN_ID"] <- "27603CA1510012_2017"
			rdata["27603CA1510006_2015","EXP_PLAN_ID"] <- "27603CA1510015_2017"
			rdata["27603CA1510007_2015","EXP_PLAN_ID"] <- "27603CA1500005_2017"
			rdata["27603CA1140003_2015","EXP_PLAN_ID"] <- "27603CA1500007_2017"
			rdata["27603CA1140004_2015","EXP_PLAN_ID"] <- "27603CA1500009_2017"
			rdata["27603CA1140007_2015","EXP_PLAN_ID"] <- "27603CA1500008_2017"
			rdata["27603CA1190002_2015","EXP_PLAN_ID"] <- "27603CA1500010_2017"
			rdata["27603CA1400005_2015","EXP_PLAN_ID"] <- "27603CA1500006_2017"
			rdata["27603CA1400006_2015","EXP_PLAN_ID"] <- "27603CA1500005_2017"
			rdata["27603CA1230012_2015","EXP_PLAN_ID"] <- "27603CA1510012_2017"
			rdata["27603CA1230013_2015","EXP_PLAN_ID"] <- "27603CA1510010_2017"
			rdata["27603CA1230014_2015","EXP_PLAN_ID"] <- "27603CA1510011_2017"
			rdata["27603CA1230017_2015","EXP_PLAN_ID"] <- "27603CA1510015_2017"
			rdata["27603CA1230018_2015","EXP_PLAN_ID"] <- "27603CA1500005_2017"
			rdata["27603CA1240001_2015","EXP_PLAN_ID"] <- "27603CA1240005_2017"
			
			# Chinese Community
			rdata["47579CA0010001_2015","EXP_PLAN_ID"] <- "47579CA0560001_2017"
			rdata["47579CA0100001_2015","EXP_PLAN_ID"] <- "47579CA0020001_2017"
			
		
	# Fix 2016
	
		# Plans missing PRJ_PLAN_ID
			# Anthem: just need to fix IDs
			# Blue Shield: 69 new plans for 2018, doesn't need PRJ_PLAN_ID (zero experience in 2016)
			# Kaiser: fix ID
			# Health Net HMO: fix IDs
			# Oscar: new in market in 2016, doesn't need PRJ_PLAN_ID (zero experience in 2016)
			# Sharp: had one new plan in 2018, so doesn't need PRJ_PLAN_ID (zero experience in 2016)
			# Western: had two new plans in 2018, so doesn't need PRJ_PLAN_ID (zero experience in 2016)
			# Chinese Community: 4 new plans, need to fix IDs of other 2
			# Remaining: Health Net PPO (too complicated to fix before adding region)
			
			# Anthem - consolidated some of their plans in 2016
				# There are 2014 multiple plans that match with a single 2016 plans
				# I'm going to put one of the IDs in (we'll get the others when we consolidate plans)
		
			rdata["27603CA1500005_2016","PRJ_PLAN_ID"] <- "27603CA1400006_2014"
			rdata["27603CA1500006_2016","PRJ_PLAN_ID"] <- "27603CA1400005_2014"
			rdata["27603CA1500007_2016","PRJ_PLAN_ID"] <- "27603CA1140003_2014"
			rdata["27603CA1500008_2016","PRJ_PLAN_ID"] <- "27603CA1140007_2014"
			rdata["27603CA1500009_2016","PRJ_PLAN_ID"] <- "27603CA1140004_2014"
			rdata["27603CA1500010_2016","PRJ_PLAN_ID"] <- "27603CA1190002_2014"
			rdata["27603CA1510010_2016","PRJ_PLAN_ID"] <- "27603CA1230013_2014"
			rdata["27603CA1510011_2016","PRJ_PLAN_ID"] <- "27603CA1230014_2014"
			rdata["27603CA1510012_2016","PRJ_PLAN_ID"] <- "27603CA1230012_2014"
			rdata["27603CA1510013_2016","PRJ_PLAN_ID"] <- "27603CA1230015_2014"
			rdata["27603CA1510014_2016","PRJ_PLAN_ID"] <- "27603CA1230016_2014"
			rdata["27603CA1510015_2016","PRJ_PLAN_ID"] <- "27603CA1230017_2014"
			
			# Kaiser - just the coinsurance gold plan is an issue
			rdata["40513CA0380013_2016","PRJ_PLAN_ID"] <- "40513CA0380002_2014"
		
			# Health Net HMO - relabel IDs
			rdata["67138CA0620022_2016","PRJ_PLAN_ID"] <- "67138CA0620014_2014"
			rdata["67138CA0620023_2016","PRJ_PLAN_ID"] <- "67138CA0620015_2014"
			rdata["67138CA0620024_2016","PRJ_PLAN_ID"] <- "67138CA0620016_2014"
			rdata["67138CA0620025_2016","PRJ_PLAN_ID"] <- "67138CA0620017_2014"
			rdata["67138CA0620026_2016","PRJ_PLAN_ID"] <- "67138CA0620018_2014"
			rdata["67138CA0620027_2016","PRJ_PLAN_ID"] <- "67138CA0620021_2014"
		
			# Health Net PPO - I did a few 
			rdata["99110CA0210012_2016","PRJ_PLAN_ID"] <- "99110CA0210007_2014"
			rdata["99110CA0210013_2016","PRJ_PLAN_ID"] <- "99110CA0210008_2014"
			rdata["99110CA0210014_2016","PRJ_PLAN_ID"] <- "99110CA0210009_2014"
			rdata["99110CA0210015_2016","PRJ_PLAN_ID"] <- "99110CA0210010_2014"
			rdata["99110CA0210016_2016","PRJ_PLAN_ID"] <- "99110CA0210011_2014"
		
			# Chinese Community
			rdata["47579CA0020001_2016","PRJ_PLAN_ID"] <- "47579CA0100001_2014"
			rdata["47579CA0560001_2016","PRJ_PLAN_ID"] <- "47579CA0010001_2014"
		
			# Sutter
			rdata["64210CA0620001_2016","PRJ_PLAN_ID"] <- "64210CA0016401_2014"
			rdata["64210CA0620002_2016","PRJ_PLAN_ID"] <- "64210CA0016402_2014"
			rdata["64210CA0620003_2016","PRJ_PLAN_ID"] <- "64210CA0016403_2014"
			rdata["64210CA0620004_2016","PRJ_PLAN_ID"] <- "64210CA0016404_2014"
			
			
			
		# Plans missing EXP_PLAN_ID
			# Anthem: Fix IDs, 1 terminated plan w/ 0 projected MM (doesn't need to be matched)
			# Chinese Community: Fix IDs
			# Kaiser: 2 plans were terminated (no projected member months), so can leave
			# Health Net: 5 plans were terminated (no projected member months)
			# Remaining: United (only in market in 2016, so can't match), Moda (same), Cigna (same)

			# Anthem
			
			rdata["27603CA1400006_2014","EXP_PLAN_ID"] <- "27603CA1500005_2016"
			rdata["27603CA1420014_2014","EXP_PLAN_ID"] <- "27603CA1500005_2016"
			rdata["27603CA1420015_2014","EXP_PLAN_ID"] <- "27603CA1500005_2016"
			rdata["27603CA1420016_2014","EXP_PLAN_ID"] <- "27603CA1500005_2016"
			rdata["27603CA1420017_2014","EXP_PLAN_ID"] <- "27603CA1500005_2016"
			rdata["27603CA1420018_2014","EXP_PLAN_ID"] <- "27603CA1500005_2016"

			rdata["27603CA1400005_2014","EXP_PLAN_ID"] <- "27603CA1500006_2016"
			rdata["27603CA1420009_2014","EXP_PLAN_ID"] <- "27603CA1500006_2016"
			rdata["27603CA1420010_2014","EXP_PLAN_ID"] <- "27603CA1500006_2016"
			rdata["27603CA1420011_2014","EXP_PLAN_ID"] <- "27603CA1500006_2016"
			rdata["27603CA1420012_2014","EXP_PLAN_ID"] <- "27603CA1500006_2016"
			rdata["27603CA1420013_2014","EXP_PLAN_ID"] <- "27603CA1500006_2016"
			
			rdata["27603CA1140003_2014","EXP_PLAN_ID"] <- "27603CA1500007_2016"
			rdata["27603CA1160008_2014","EXP_PLAN_ID"] <- "27603CA1500007_2016"
			rdata["27603CA1160009_2014","EXP_PLAN_ID"] <- "27603CA1500007_2016"
			rdata["27603CA1160010_2014","EXP_PLAN_ID"] <- "27603CA1500007_2016"
			rdata["27603CA1160011_2014","EXP_PLAN_ID"] <- "27603CA1500007_2016"
			rdata["27603CA1160012_2014","EXP_PLAN_ID"] <- "27603CA1500007_2016"
			
			rdata["27603CA1140007_2014","EXP_PLAN_ID"] <- "27603CA1500008_2016"
			rdata["27603CA1160013_2014","EXP_PLAN_ID"] <- "27603CA1500008_2016"
			rdata["27603CA1160014_2014","EXP_PLAN_ID"] <- "27603CA1500008_2016"
			rdata["27603CA1160015_2014","EXP_PLAN_ID"] <- "27603CA1500008_2016"
			rdata["27603CA1160016_2014","EXP_PLAN_ID"] <- "27603CA1500008_2016"
			rdata["27603CA1160017_2014","EXP_PLAN_ID"] <- "27603CA1500008_2016"
			
			rdata["27603CA1140004_2014","EXP_PLAN_ID"] <- "27603CA1500009_2016"
			rdata["27603CA1160018_2014","EXP_PLAN_ID"] <- "27603CA1500009_2016"
			rdata["27603CA1160019_2014","EXP_PLAN_ID"] <- "27603CA1500009_2016"
			rdata["27603CA1160020_2014","EXP_PLAN_ID"] <- "27603CA1500009_2016"
			rdata["27603CA1160021_2014","EXP_PLAN_ID"] <- "27603CA1500009_2016"
			rdata["27603CA1160022_2014","EXP_PLAN_ID"] <- "27603CA1500009_2016"
			
			rdata["27603CA1190002_2014","EXP_PLAN_ID"] <- "27603CA1500010_2016"
			rdata["27603CA1200002_2014","EXP_PLAN_ID"] <- "27603CA1500010_2016"
			rdata["27603CA1200003_2014","EXP_PLAN_ID"] <- "27603CA1500010_2016"
			rdata["27603CA1200004_2014","EXP_PLAN_ID"] <- "27603CA1500010_2016"
			rdata["27603CA1200005_2014","EXP_PLAN_ID"] <- "27603CA1500010_2016"
			rdata["27603CA1200006_2014","EXP_PLAN_ID"] <- "27603CA1500010_2016"
			
			rdata["27603CA1230013_2014","EXP_PLAN_ID"] <- "27603CA1510010_2016"
			rdata["27603CA1250014_2014","EXP_PLAN_ID"] <- "27603CA1510010_2016"
			rdata["27603CA1250015_2014","EXP_PLAN_ID"] <- "27603CA1510010_2016"
			rdata["27603CA1250016_2014","EXP_PLAN_ID"] <- "27603CA1510010_2016"
			rdata["27603CA1250017_2014","EXP_PLAN_ID"] <- "27603CA1510010_2016"
			rdata["27603CA1250018_2014","EXP_PLAN_ID"] <- "27603CA1510010_2016"
			
			rdata["27603CA1230014_2014","EXP_PLAN_ID"] <- "27603CA1510011_2016"
			rdata["27603CA1250019_2014","EXP_PLAN_ID"] <- "27603CA1510011_2016"
			rdata["27603CA1250020_2014","EXP_PLAN_ID"] <- "27603CA1510011_2016"
			rdata["27603CA1250021_2014","EXP_PLAN_ID"] <- "27603CA1510011_2016"
			rdata["27603CA1250022_2014","EXP_PLAN_ID"] <- "27603CA1510011_2016"
			rdata["27603CA1250023_2014","EXP_PLAN_ID"] <- "27603CA1510011_2016"
			
			rdata["27603CA1230012_2014","EXP_PLAN_ID"] <- "27603CA1510012_2016"
			rdata["27603CA1250024_2014","EXP_PLAN_ID"] <- "27603CA1510012_2016"
			rdata["27603CA1250025_2014","EXP_PLAN_ID"] <- "27603CA1510012_2016"
			rdata["27603CA1250026_2014","EXP_PLAN_ID"] <- "27603CA1510012_2016"
			rdata["27603CA1250027_2014","EXP_PLAN_ID"] <- "27603CA1510012_2016"
			rdata["27603CA1250028_2014","EXP_PLAN_ID"] <- "27603CA1510012_2016"
			
			rdata["27603CA1230015_2014","EXP_PLAN_ID"] <- "27603CA1510013_2016"
			rdata["27603CA1250029_2014","EXP_PLAN_ID"] <- "27603CA1510013_2016"
			rdata["27603CA1250030_2014","EXP_PLAN_ID"] <- "27603CA1510013_2016"
			rdata["27603CA1250031_2014","EXP_PLAN_ID"] <- "27603CA1510013_2016"
			rdata["27603CA1250032_2014","EXP_PLAN_ID"] <- "27603CA1510013_2016"
			rdata["27603CA1250033_2014","EXP_PLAN_ID"] <- "27603CA1510013_2016"
			
			rdata["27603CA1230016_2014","EXP_PLAN_ID"] <- "27603CA1510014_2016"
			rdata["27603CA1250034_2014","EXP_PLAN_ID"] <- "27603CA1510014_2016"
			rdata["27603CA1250035_2014","EXP_PLAN_ID"] <- "27603CA1510014_2016"
			rdata["27603CA1250036_2014","EXP_PLAN_ID"] <- "27603CA1510014_2016"
			rdata["27603CA1250037_2014","EXP_PLAN_ID"] <- "27603CA1510014_2016"
			rdata["27603CA1250038_2014","EXP_PLAN_ID"] <- "27603CA1510014_2016"
			
			rdata["27603CA1230017_2014","EXP_PLAN_ID"] <- "27603CA1510015_2016"
			rdata["27603CA1250039_2014","EXP_PLAN_ID"] <- "27603CA1510015_2016"
			rdata["27603CA1250040_2014","EXP_PLAN_ID"] <- "27603CA1510015_2016"
			rdata["27603CA1250041_2014","EXP_PLAN_ID"] <- "27603CA1510015_2016"
			rdata["27603CA1250042_2014","EXP_PLAN_ID"] <- "27603CA1510015_2016"
			rdata["27603CA1250043_2014","EXP_PLAN_ID"] <- "27603CA1510015_2016"
			
			rdata["27603CA1230018_2014","EXP_PLAN_ID"] <- "27603CA1500005_2016"
			rdata["27603CA1250013_2014","EXP_PLAN_ID"] <- "27603CA1500005_2016"
			rdata["27603CA1250044_2014","EXP_PLAN_ID"] <- "27603CA1500005_2016"
			rdata["27603CA1250045_2014","EXP_PLAN_ID"] <- "27603CA1500005_2016"
			rdata["27603CA1250046_2014","EXP_PLAN_ID"] <- "27603CA1500005_2016"
			rdata["27603CA1250047_2014","EXP_PLAN_ID"] <- "27603CA1500005_2016"
			
			# Health Net PPO
			rdata["99110CA0210007_2014","EXP_PLAN_ID"] <- "99110CA0210012_2016"
			rdata["99110CA0210008_2014","EXP_PLAN_ID"] <- "99110CA0210013_2016"
			rdata["99110CA0210009_2014","EXP_PLAN_ID"] <- "99110CA0210014_2016"
			rdata["99110CA0210010_2014","EXP_PLAN_ID"] <- "99110CA0210015_2016"
			rdata["99110CA0210011_2014","EXP_PLAN_ID"] <- "99110CA0210016_2016"
			
			# Chinese Community
			rdata["47579CA0100001_2014","EXP_PLAN_ID"] <- "47579CA0020001_2016"
			rdata["47579CA0010001_2014","EXP_PLAN_ID"] <- "47579CA0560001_2016"
			
			# Sutter
			rdata["64210CA0016401_2014","EXP_PLAN_ID"] <- "64210CA0620001_2016"
			rdata["64210CA0016402_2014","EXP_PLAN_ID"] <- "64210CA0620002_2016"
			rdata["64210CA0016403_2014","EXP_PLAN_ID"] <- "64210CA0620003_2016"
			rdata["64210CA0016404_2014","EXP_PLAN_ID"] <- "64210CA0620004_2016"
			
	# Fix 2015
	
		# Plans Missing PRJ_PLAN_ID
			# Anthem: 8 new HMO plans for 2017, which do not need PRJ_PLAN_ID (zero experience in 2015)
				# 1 plan is a catch all for experience? - not sure what to do with this
				# All other plans I fixed the IDs
			# Blue Shield: 54 new plans for 2017, doesn't need PRJ_PLAN_ID (zero experience in 2015)
			# Health Net HMO: 9 new plans for 2017, doesn't need PRJ_PLAN_ID (zero experience in 2015)
			# Health Net PPO: fix ID for 5 plans, 90 new plans for 2017, doesn't need PRJ_PLAN_ID (zero experience in 2015)
			# Kaiser: 2 new plans for 2017, doesn't need PRJ_PLAN_ID (zero experience in 2015)
			# LA Care: Fix IDs
			# Oscar: wasn't in market in 2015, records just used for projections for 2017
			# Chinese Community: Fix IDs
			# Cigna: all plans were new in 2017, so can leave (zero experience in 2015)
			# Remaining: 1 Anthem plan, Oscar, Sutter (no data for 2013)
			
		# Anthem	
		rdata["27603CA1500001_2015","PRJ_PLAN_ID"] <- "27603CA1140003_2013"
		rdata["27603CA1500002_2015","PRJ_PLAN_ID"] <- "27603CA1140007_2013"
		rdata["27603CA1500003_2015","PRJ_PLAN_ID"] <- "27603CA1140004_2013"
		rdata["27603CA1500004_2015","PRJ_PLAN_ID"] <- "27603CA1190002_2013"
		rdata["27603CA1520001_2015","PRJ_PLAN_ID"] <- "27603CA1400005_2013"
		rdata["27603CA1520002_2015","PRJ_PLAN_ID"] <- "27603CA1400006_2013"

		rdata["27603CA1510001_2015","PRJ_PLAN_ID"] <- "27603CA1250014_2013"
		rdata["27603CA1510002_2015","PRJ_PLAN_ID"] <- "27603CA1250019_2013"
		rdata["27603CA1510003_2015","PRJ_PLAN_ID"] <- "27603CA1230024_2013"
		rdata["27603CA1510004_2015","PRJ_PLAN_ID"] <- "27603CA1250029_2013"
		rdata["27603CA1510005_2015","PRJ_PLAN_ID"] <- "27603CA1250034_2013"
		rdata["27603CA1510006_2015","PRJ_PLAN_ID"] <- "48962CA1230010_2013"
		rdata["27603CA1510007_2015","PRJ_PLAN_ID"] <- "48962CA1230015_2013"

		# Health Net PPO
		rdata["99110CA0210012_2015","PRJ_PLAN_ID"] <- "99110CA0350001_2013" 
		rdata["99110CA0210013_2015","PRJ_PLAN_ID"] <- "99110CA0350002_2013" 
		rdata["99110CA0210014_2015","PRJ_PLAN_ID"] <- "99110CA0350003_2013" 
		rdata["99110CA0210015_2015","PRJ_PLAN_ID"] <- "99110CA0350004_2013" 
		rdata["99110CA0210016_2015","PRJ_PLAN_ID"] <- "99110CA0350005_2013"

		# LA Care	
		rdata["92815CA0010004_2015","PRJ_PLAN_ID"] <- "92815CA001-004_2013"
		rdata["92815CA0010005_2015","PRJ_PLAN_ID"] <- "92815CA001-005_2013"
		rdata["92815CA0010006_2015","PRJ_PLAN_ID"] <- "92815CA001-006_2013"
		rdata["92815CA0010007_2015","PRJ_PLAN_ID"] <- "92815CA001-007_2013"
		rdata["92815CA0010008_2015","PRJ_PLAN_ID"] <- "92815CA001-008_2013"
		
		# Chinese Community
		rdata["47579CA0180001_2015","PRJ_PLAN_ID"] <- "47579CA0180000_2013"
		rdata["47579CA0200001_2015","PRJ_PLAN_ID"] <- "47579CA0200000_2013"
		rdata["47579CA0220001_2015","PRJ_PLAN_ID"] <- "47579CA0220000_2013"
		rdata["47579CA0240001_2015","PRJ_PLAN_ID"] <- "47579CA0240000_2013"
		rdata["47579CA0260001_2015","PRJ_PLAN_ID"] <- "47579CA0260000_2013"
		rdata["47579CA0270001_2015","PRJ_PLAN_ID"] <- "47579CA0180000_2013"
		rdata["47579CA0290001_2015","PRJ_PLAN_ID"] <- "47579CA0200000_2013"
		rdata["47579CA0310001_2015","PRJ_PLAN_ID"] <- "47579CA0220000_2013"
		rdata["47579CA0330001_2015","PRJ_PLAN_ID"] <- "47579CA0240000_2013"
		rdata["47579CA0350001_2015","PRJ_PLAN_ID"] <- "47579CA0260000_2013"
		rdata["47579CA0010001_2015","PRJ_PLAN_ID"] <- "47579CA0010000_2013"
		rdata["47579CA0100001_2015","PRJ_PLAN_ID"] <- "47579CA0100000_2013"
		rdata["47579CA0110001_2015","PRJ_PLAN_ID"] <- "47579CA0110000_2013"
		rdata["47579CA0120001_2015","PRJ_PLAN_ID"] <- "47579CA0120000_2013"
		
		# Plans Missing EXP_PLAN_ID
			# Anthem: Fix IDs, can toss 15 plans (have essentially zero projected member months)
			# Blue Shield: 1 plan with zero projected member months - can delete
			# Kaiser: 1 plan with zero projected member months - can delete
			# LA Care: Fix IDs
			# Health Net: 122 plans with zero projected member months - can delete
			# Time: 134 plans with zero projected member months - can delete
			# Cigna: Fix IDs
			# Chinese Community: Fix IDs
			
			drop_firms <- c("Blue_Shield","Kaiser","Health_Net_HMO","Health_Net_PPO","Time Insurance Company")
			rdata <- rdata[!(rdata$COMPANY %in% drop_firms & is.na(rdata$EXP_PLAN_ID) & rdata$year == 2013),]
			
		# Anthem
		rdata["27603CA1140003_2013","EXP_PLAN_ID"] <- "27603CA1500001_2015"
		rdata["27603CA1160008_2013","EXP_PLAN_ID"] <- "27603CA1500001_2015"
		rdata["27603CA1160009_2013","EXP_PLAN_ID"] <- "27603CA1500001_2015"
		rdata["27603CA1160010_2013","EXP_PLAN_ID"] <- "27603CA1500001_2015"
		rdata["27603CA1160011_2013","EXP_PLAN_ID"] <- "27603CA1500001_2015"
		rdata["27603CA1160012_2013","EXP_PLAN_ID"] <- "27603CA1500001_2015"
				
		rdata["27603CA1140007_2013","EXP_PLAN_ID"] <- "27603CA1500002_2015"
		rdata["27603CA1160013_2013","EXP_PLAN_ID"] <- "27603CA1500002_2015"
		rdata["27603CA1160014_2013","EXP_PLAN_ID"] <- "27603CA1500002_2015"
		rdata["27603CA1160015_2013","EXP_PLAN_ID"] <- "27603CA1500002_2015"
		rdata["27603CA1160016_2013","EXP_PLAN_ID"] <- "27603CA1500002_2015"
		rdata["27603CA1160017_2013","EXP_PLAN_ID"] <- "27603CA1500002_2015"
					
		rdata["27603CA1140004_2013","EXP_PLAN_ID"] <- "27603CA1500003_2015"
		rdata["27603CA1160018_2013","EXP_PLAN_ID"] <- "27603CA1500003_2015"
		rdata["27603CA1160019_2013","EXP_PLAN_ID"] <- "27603CA1500003_2015"
		rdata["27603CA1160020_2013","EXP_PLAN_ID"] <- "27603CA1500003_2015"
		rdata["27603CA1160021_2013","EXP_PLAN_ID"] <- "27603CA1500003_2015"
		rdata["27603CA1160022_2013","EXP_PLAN_ID"] <- "27603CA1500003_2015"
						
		rdata["27603CA1190002_2013","EXP_PLAN_ID"] <- "27603CA1500004_2015"
		rdata["27603CA1200002_2013","EXP_PLAN_ID"] <- "27603CA1500004_2015"
		rdata["27603CA1200003_2013","EXP_PLAN_ID"] <- "27603CA1500004_2015"
		rdata["27603CA1200004_2013","EXP_PLAN_ID"] <- "27603CA1500004_2015"
		rdata["27603CA1200005_2013","EXP_PLAN_ID"] <- "27603CA1500004_2015"
		rdata["27603CA1200006_2013","EXP_PLAN_ID"] <- "27603CA1500004_2015"
			
		rdata["27603CA1400005_2013","EXP_PLAN_ID"] <- "27603CA1520001_2015"
		rdata["27603CA1420009_2013","EXP_PLAN_ID"] <- "27603CA1520001_2015"
		rdata["27603CA1420010_2013","EXP_PLAN_ID"] <- "27603CA1520001_2015"
		rdata["27603CA1420011_2013","EXP_PLAN_ID"] <- "27603CA1520001_2015"
		rdata["27603CA1420012_2013","EXP_PLAN_ID"] <- "27603CA1520001_2015"
		rdata["27603CA1420013_2013","EXP_PLAN_ID"] <- "27603CA1520001_2015"
		
		rdata["27603CA1400006_2013","EXP_PLAN_ID"] <- "27603CA1520002_2015"
		rdata["27603CA1420014_2013","EXP_PLAN_ID"] <- "27603CA1520002_2015"
		rdata["27603CA1420015_2013","EXP_PLAN_ID"] <- "27603CA1520002_2015"
		rdata["27603CA1420016_2013","EXP_PLAN_ID"] <- "27603CA1520002_2015"
		rdata["27603CA1420017_2013","EXP_PLAN_ID"] <- "27603CA1520002_2015"
		rdata["27603CA1420018_2013","EXP_PLAN_ID"] <- "27603CA1520002_2015"

		rdata["27603CA1230012_2013","EXP_PLAN_ID"] <- "27603CA1510003_2015"
		rdata["27603CA1230013_2013","EXP_PLAN_ID"] <- "27603CA1510003_2015"
		rdata["27603CA1250024_2013","EXP_PLAN_ID"] <- "27603CA1510003_2015"
		rdata["27603CA1250025_2013","EXP_PLAN_ID"] <- "27603CA1510003_2015"
		rdata["27603CA1250026_2013","EXP_PLAN_ID"] <- "27603CA1510003_2015"
		rdata["27603CA1250027_2013","EXP_PLAN_ID"] <- "27603CA1510003_2015"
		rdata["27603CA1250028_2013","EXP_PLAN_ID"] <- "27603CA1510003_2015"
		
		rdata["27603CA1250014_2013","EXP_PLAN_ID"] <- "27603CA1510001_2015"
		rdata["27603CA1250015_2013","EXP_PLAN_ID"] <- "27603CA1510001_2015"
		rdata["27603CA1250016_2013","EXP_PLAN_ID"] <- "27603CA1510001_2015"
		rdata["27603CA1250017_2013","EXP_PLAN_ID"] <- "27603CA1510001_2015"
		rdata["27603CA1250018_2013","EXP_PLAN_ID"] <- "27603CA1510001_2015"
		
		rdata["27603CA1250019_2013","EXP_PLAN_ID"] <- "27603CA1510002_2015"
		rdata["27603CA1250020_2013","EXP_PLAN_ID"] <- "27603CA1510002_2015"
		rdata["27603CA1250021_2013","EXP_PLAN_ID"] <- "27603CA1510002_2015"
		rdata["27603CA1250022_2013","EXP_PLAN_ID"] <- "27603CA1510002_2015"
		rdata["27603CA1250023_2013","EXP_PLAN_ID"] <- "27603CA1510002_2015"
		
		rdata["48962CA1210006_2013","EXP_PLAN_ID"] <- "27603CA1510006_2015"
		rdata["27603CA1230017_2013","EXP_PLAN_ID"] <- "27603CA1510006_2015"
		rdata["48962CA1230010_2013","EXP_PLAN_ID"] <- "27603CA1510006_2015"
		rdata["48962CA1230011_2013","EXP_PLAN_ID"] <- "27603CA1510006_2015"
		rdata["48962CA1230012_2013","EXP_PLAN_ID"] <- "27603CA1510006_2015"
		rdata["48962CA1230013_2013","EXP_PLAN_ID"] <- "27603CA1510006_2015"
		rdata["48962CA1230014_2013","EXP_PLAN_ID"] <- "27603CA1510006_2015"
		rdata["27603CA1250039_2013","EXP_PLAN_ID"] <- "27603CA1510006_2015"
		rdata["27603CA1250040_2013","EXP_PLAN_ID"] <- "27603CA1510006_2015"
		rdata["27603CA1250041_2013","EXP_PLAN_ID"] <- "27603CA1510006_2015"
		rdata["27603CA1250042_2013","EXP_PLAN_ID"] <- "27603CA1510006_2015"
		rdata["27603CA1250043_2013","EXP_PLAN_ID"] <- "27603CA1510006_2015"
		
		rdata["27603CA1250034_2013","EXP_PLAN_ID"] <- "27603CA1510005_2015"
		rdata["27603CA1250035_2013","EXP_PLAN_ID"] <- "27603CA1510005_2015"
		rdata["27603CA1250036_2013","EXP_PLAN_ID"] <- "27603CA1510005_2015"
		rdata["27603CA1250037_2013","EXP_PLAN_ID"] <- "27603CA1510005_2015"
		rdata["27603CA1250038_2013","EXP_PLAN_ID"] <- "27603CA1510005_2015"
		
		rdata["27603CA1250029_2013","EXP_PLAN_ID"] <- "27603CA1510004_2015"
		rdata["27603CA1250030_2013","EXP_PLAN_ID"] <- "27603CA1510004_2015"
		rdata["27603CA1250031_2013","EXP_PLAN_ID"] <- "27603CA1510004_2015"
		rdata["27603CA1250032_2013","EXP_PLAN_ID"] <- "27603CA1510004_2015"
		rdata["27603CA1250033_2013","EXP_PLAN_ID"] <- "27603CA1510004_2015"
		
		rdata["48962CA1230015_2013","EXP_PLAN_ID"] <- "27603CA1510007_2015"
		rdata["48962CA1230016_2013","EXP_PLAN_ID"] <- "27603CA1510007_2015"
		rdata["48962CA1230017_2013","EXP_PLAN_ID"] <- "27603CA1510007_2015"
		rdata["48962CA1230018_2013","EXP_PLAN_ID"] <- "27603CA1510007_2015"
		rdata["48962CA1230019_2013","EXP_PLAN_ID"] <- "27603CA1510007_2015"
		rdata["27603CA1250013_2013","EXP_PLAN_ID"] <- "27603CA1510007_2015"
		rdata["27603CA1250044_2013","EXP_PLAN_ID"] <- "27603CA1510007_2015"
		rdata["27603CA1250045_2013","EXP_PLAN_ID"] <- "27603CA1510007_2015"
		rdata["27603CA1250046_2013","EXP_PLAN_ID"] <- "27603CA1510007_2015"
		rdata["27603CA1250047_2013","EXP_PLAN_ID"] <- "27603CA1510007_2015"
		
		rdata <- rdata[!(rdata$COMPANY %in% c("Anthem") & is.na(rdata$EXP_PLAN_ID) & rdata$year == 2013),]
			
			
		# LA Care
		rdata["92815CA001-004_2013","EXP_PLAN_ID"] <- "92815CA0010004_2015"
		rdata["92815CA001-005_2013","EXP_PLAN_ID"] <- "92815CA0010005_2015"
		rdata["92815CA001-006_2013","EXP_PLAN_ID"] <- "92815CA0010006_2015"
		rdata["92815CA001-007_2013","EXP_PLAN_ID"] <- "92815CA0010007_2015"
		rdata["92815CA001-008_2013","EXP_PLAN_ID"] <- "92815CA0010008_2015"
	
		# Chinese Community
		rdata["47579CA0180000_2013","EXP_PLAN_ID"] <- "47579CA0180001_2015"
		rdata["47579CA0200000_2013","EXP_PLAN_ID"] <- "47579CA0200001_2015"
		rdata["47579CA0220000_2013","EXP_PLAN_ID"] <- "47579CA0220001_2015"
		rdata["47579CA0240000_2013","EXP_PLAN_ID"] <- "47579CA0240001_2015"
		rdata["47579CA0260000_2013","EXP_PLAN_ID"] <- "47579CA0260001_2015"
		rdata["47579CA0010000_2013","EXP_PLAN_ID"] <- "47579CA0010001_2015"
		rdata["47579CA0100000_2013","EXP_PLAN_ID"] <- "47579CA0100001_2015"
		rdata["47579CA0110000_2013","EXP_PLAN_ID"] <- "47579CA0110001_2015"
		rdata["47579CA0120000_2013","EXP_PLAN_ID"] <- "47579CA0120001_2015"
		
		# Cigna
		rdata["40025CA0020007_2013","EXP_PLAN_ID"] <- "40025CA0020006_2015"
		rdata["40025CA0020010_2013","EXP_PLAN_ID"] <- "40025CA0020008_2015"
		
	# Fix 2014

		# Plans Missing PRJ_PLAN_ID
			# Blue Shield: 3 new plans for 2016, doesn't need PRJ_PLAN_ID (zero experience in 2014)
			# Health Net HMO: 4 new plans for 2016, fix the IDs for the other plans
			# Kaiser: 2 new plans for 2016, doesn't need PRJ_PLAN_ID (zero experience in 2014)
			# Oscar: new in market for 2016, doesn't need PRJ_PLAN_ID
			# LA Care: fix IDs
			# Chinese Community: fix IDs
			# Wetsern: 8 new plans for 2016 (zero experience in 2014), fix IDs for other 2 off-exchange plans
			# Remaining: Oscar, United (only in market in 2016, so can't match), Sutter and Moda (no data for 2014)
		
			# Anthem
			rdata["27603CA1140007_2014","PRJ_PLAN_ID"] <- "27603CA1140005_2012" 
			rdata[c("27603CA1150020_2014","27603CA1150021_2014","27603CA1150022_2014","27603CA1150023_2014","27603CA1150024_2014",
				"27603CA1150025_2014","27603CA1150026_2014","27603CA1150027_2014"),"PRJ_PLAN_ID"] <- "27603CA1150002_2012" 
			rdata[c("27603CA1160013_2014","27603CA1160014_2014","27603CA1160015_2014","27603CA1160016_2014","27603CA1160017_2014"),"PRJ_PLAN_ID"] <- "27603CA1160004_2012"

			rdata[c("27603CA1150012_2014","27603CA1150013_2014","27603CA1150014_2014","27603CA1150015_2014","27603CA1150016_2014",
				"27603CA1150017_2014","27603CA1150018_2014","27603CA1150019_2014"),"PRJ_PLAN_ID"] <- "27603CA1150001_2012" 
			rdata[c("27603CA1400006_2014"),"PRJ_PLAN_ID"] <- "27603CA1400003_2012" 
			rdata[c("27603CA1420014_2014","27603CA1420015_2014","27603CA1420016_2014","27603CA1420017_2014","27603CA1420018_2014"),"PRJ_PLAN_ID"] <- "27603CA1420003_2012"

			rdata[c("27603CA1150004_2014","27603CA1150005_2014","27603CA1150006_2014","27603CA1150007_2014","27603CA1150008_2014",
				"27603CA1150009_2014","27603CA1150010_2014","27603CA1150011_2014"),"PRJ_PLAN_ID"] <- "27603CA1150001_2012" 
			rdata[c("27603CA1400005_2014"),"PRJ_PLAN_ID"] <- "27603CA1400003_2012" 
			rdata[c("27603CA1420009_2014","27603CA1420010_2014","27603CA1420011_2014","27603CA1420012_2014","27603CA1420013_2014"),"PRJ_PLAN_ID"] <- "27603CA1420003_2012"
		
			rdata[c("27603CA1140004_2014"),"PRJ_PLAN_ID"] <- "27603CA1140002_2012" 
			rdata[c("27603CA1160018_2014","27603CA1160019_2014","27603CA1160020_2014","27603CA1160021_2014","27603CA1160022_2014"),"PRJ_PLAN_ID"] <- "27603CA1160001_2012" 
			rdata[c("27603CA1190002_2014"),"PRJ_PLAN_ID"] <- "27603CA1190001_2012" 
			rdata[c("27603CA1200002_2014","27603CA1200003_2014","27603CA1200004_2014","27603CA1200005_2014","27603CA1200006_2014"),"PRJ_PLAN_ID"] <- "27603CA1200001_2012"
		
			rdata[c("27603CA1140003_2014"),"PRJ_PLAN_ID"] <- "27603CA1140001_2012" 
			rdata[c("27603CA1160008_2014","27603CA1160009_2014","27603CA1160010_2014","27603CA1160011_2014","27603CA1160012_2014"),"PRJ_PLAN_ID"] <- "27603CA1160007_2012"
		 
			rdata[c("27603CA1230018_2014"),"PRJ_PLAN_ID"] <- "27603CA1230003_2012"
			rdata[c("27603CA1250013_2014","27603CA1250044_2014","27603CA1250045_2014","27603CA1250046_2014","27603CA1250047_2014"),"PRJ_PLAN_ID"] <- "27603CA1250011_2012"

			rdata[c("27603CA1230015_2014","27603CA1230016_2014","27603CA1230017_2014"),"PRJ_PLAN_ID"] <- "27603CA1230002_2012"
			rdata[c("27603CA1250029_2014","27603CA1250030_2014","27603CA1250031_2014","27603CA1250032_2014","27603CA1250033_2014",
					"27603CA1250034_2014","27603CA1250035_2014","27603CA1250036_2014","27603CA1250037_2014","27603CA1250038_2014",
					"27603CA1250039_2014","27603CA1250040_2014","27603CA1250041_2014","27603CA1250042_2014","27603CA1250043_2014"),"PRJ_PLAN_ID"] <- "27603CA1250006_2012"

			rdata[c("27603CA1230012_2014","27603CA1230013_2014","27603CA1230014_2014"),"PRJ_PLAN_ID"] <- "27603CA1230005_2012"
			rdata[c("27603CA1250014_2014","27603CA1250015_2014","27603CA1250016_2014","27603CA1250017_2014","27603CA1250018_2014",
				"27603CA1250019_2014","27603CA1250020_2014","27603CA1250021_2014","27603CA1250022_2014","27603CA1250023_2014",
				"27603CA1250024_2014","27603CA1250025_2014","27603CA1250026_2014","27603CA1250027_2014","27603CA1250028_2014"),"PRJ_PLAN_ID"] <- "27603CA1250001_2012"
		  
			rdata["27603CA1111111_2014","PRJ_PLAN_ID"] <- "27603CA1230001_2012"
			
			# Health Net HMO
			platinum_ids <- c("67138CA0520004_2014","67138CA0520005_2014","67138CA0520006_2014","67138CA0520007_2014","67138CA0520008_2014",
				"67138CA0520019_2014","67138CA0620004_2014","67138CA0620005_2014","67138CA0620006_2014","67138CA0620007_2014",
				"67138CA0620008_2014","67138CA0620019_2014","67138CA0630003_2014","67138CA0640001_2014")
			gold_ids <- c("67138CA0520009_2014","67138CA0520010_2014","67138CA0520011_2014","67138CA0520012_2014","67138CA0520013_2014",
				"67138CA0520020_2014","67138CA0620009_2014","67138CA0620010_2014","67138CA0620011_2014","67138CA0620012_2014",
				"67138CA0620013_2014","67138CA0620020_2014","67138CA0630004_2014","67138CA0640002_2014")
			silver_ids <- c("67138CA0520014_2014","67138CA0520015_2014","67138CA0520016_2014","67138CA0520017_2014","67138CA0520018_2014",
				"67138CA0520021_2014","67138CA0620014_2014","67138CA0620015_2014","67138CA0620016_2014","67138CA0620017_2014",
				"67138CA0620018_2014","67138CA0620021_2014","67138CA0630005_2014","67138CA0640003_2014")

			rdata[platinum_ids,"PRJ_PLAN_ID"] <- "67138CA0520002_2012" 
			rdata[gold_ids,"PRJ_PLAN_ID"] <- "67138CA0520003_2012" 
			rdata[silver_ids,"PRJ_PLAN_ID"] <- "67138CA0520001_2012" 
		
			# Health Net PPO
			platinum_ids <- c("99110CA0290001_2014","99110CA0340006_2014","99110CA0340007_2014","99110CA0340008_2014","99110CA0340009_2014","99110CA0340010_2014","99110CA0340011_2014")
			gold_ids <- c("99110CA0290002_2014","99110CA0340012_2014","99110CA0340013_2014","99110CA0340014_2014","99110CA0340015_2014","99110CA0340016_2014","99110CA0340017_2014")
			silver_ids <- c("99110CA0290003_2014","99110CA0340018_2014","99110CA0340019_2014","99110CA0340020_2014","99110CA0340021_2014","99110CA0340022_2014","99110CA0340023_2014") 
			bronze_ids <- c("99110CA0290004_2014","99110CA0340024_2014","99110CA0340025_2014","99110CA0340026_2014","99110CA0340027_2014","99110CA0340028_2014","99110CA0340029_2014")
			catastrophic_ids <- c("99110CA0290005_2014","99110CA0340030_2014","99110CA0340031_2014","99110CA0340032_2014","99110CA0340033_2014","99110CA0340034_2014","99110CA0340035_2014") 
			
			rdata[platinum_ids,"PRJ_PLAN_ID"] <- "99110CA0290001_2012" 
			rdata[gold_ids,"PRJ_PLAN_ID"] <- "99110CA0290002_2012" 
			rdata[silver_ids,"PRJ_PLAN_ID"] <- "99110CA0290003_2012" 
			rdata[bronze_ids,"PRJ_PLAN_ID"] <- "99110CA0290004_2012" 
			rdata[catastrophic_ids,"PRJ_PLAN_ID"] <- "99110CA0290005_2012"

			platinum_ids <- c("99110CA0210007_2014","99110CA0350006_2014","99110CA0350007_2014","99110CA0350008_2014","99110CA0350009_2014","99110CA0350010_2014","99110CA0350011_2014",
							"99110CA0350012_2014","99110CA0350013_2014","99110CA0350046_2014","99110CA0350047_2014","99110CA0350048_2014","99110CA0350049_2014")
			gold_ids <- c("99110CA0210008_2014","99110CA0350014_2014","99110CA0350015_2014","99110CA0350016_2014","99110CA0350017_2014","99110CA0350018_2014","99110CA0350019_2014",
							"99110CA0350020_2014","99110CA0350021_2014","99110CA0350050_2014","99110CA0350051_2014","99110CA0350052_2014","99110CA0350053_2014")
			silver_ids <- c("99110CA0210009_2014","99110CA0350022_2014","99110CA0350023_2014","99110CA0350024_2014","99110CA0350025_2014","99110CA0350026_2014","99110CA0350027_2014",
							"99110CA0350028_2014","99110CA0350029_2014","99110CA0350054_2014","99110CA0350055_2014","99110CA0350056_2014","99110CA0350057_2014") 
			bronze_ids <- c("99110CA0210010_2014","99110CA0350030_2014","99110CA0350031_2014","99110CA0350032_2014","99110CA0350033_2014","99110CA0350034_2014","99110CA0350035_2014",
							"99110CA0350036_2014","99110CA0350037_2014","99110CA0350058_2014","99110CA0350059_2014","99110CA0350060_2014","99110CA0350061_2014") 
			catastrophic_ids <- c("99110CA0210011_2014","99110CA0350038_2014","99110CA0350039_2014","99110CA0350040_2014","99110CA0350041_2014","99110CA0350042_2014","99110CA0350043_2014",
								"99110CA0350044_2014","99110CA0350045_2014","99110CA0350062_2014","99110CA0350063_2014","99110CA0350064_2014","99110CA0350065_2014") 
		
			rdata[platinum_ids,"PRJ_PLAN_ID"] <- "99110CA0210007_2012" 
			rdata[gold_ids,"PRJ_PLAN_ID"] <- "99110CA0210008_2012" 
			rdata[silver_ids,"PRJ_PLAN_ID"] <- "99110CA0210009_2012" 
			rdata[bronze_ids,"PRJ_PLAN_ID"] <- "99110CA0210010_2012"
			rdata[catastrophic_ids,"PRJ_PLAN_ID"] <- "99110CA0210011_2012"
 


		
			# LA Care	
			rdata["92815CA0010004_2014","PRJ_PLAN_ID"] <- "92815CA001004_2012"
			rdata["92815CA0010005_2014","PRJ_PLAN_ID"] <- "92815CA001005_2012"
			rdata["92815CA0010006_2014","PRJ_PLAN_ID"] <- "92815CA001006_2012"
			rdata["92815CA0010007_2014","PRJ_PLAN_ID"] <- "92815CA001007_2012"
			rdata["92815CA0010008_2014","PRJ_PLAN_ID"] <- "92815CA001008_2012"
			
			# Chinese Community
			rdata["47579CA0270001_2014","PRJ_PLAN_ID"] <- "47579CA0180001_2012"
			rdata["47579CA0290001_2014","PRJ_PLAN_ID"] <- "47579CA0200001_2012"
			rdata["47579CA0310001_2014","PRJ_PLAN_ID"] <- "47579CA0220001_2012"
			rdata["47579CA0330001_2014","PRJ_PLAN_ID"] <- "47579CA0240001_2012"
			rdata["47579CA0350001_2014","PRJ_PLAN_ID"] <- "47579CA0260001_2012"
			rdata["47579CA0010001_2014","PRJ_PLAN_ID"] <- "47579CA0180001_2012"
			rdata["47579CA0100001_2014","PRJ_PLAN_ID"] <- "47579CA0220001_2012"
			rdata["47579CA0110001_2014","PRJ_PLAN_ID"] <- "47579CA0180001_2012"
			rdata["47579CA0120001_2014","PRJ_PLAN_ID"] <- "47579CA0220001_2012"
			
			# Western
			rdata["93689CA0050003_2014","PRJ_PLAN_ID"] <- "93689CA0120001_2012"
			rdata["93689CA0050004_2014","PRJ_PLAN_ID"] <- "93689CA0130002_2012"
		
		# Plans Missing EXP_PLAN_ID
			# Anthem: Fix IDs, 11 plans left over with 0 projected member months
			# Blue Shield: Fix IDs for 4 plans
			# kaiser: 1 plan w/ 0 projected member months
			# Sharp: Fix IDs
			# Remaining: Contra Costa (won't be able to match b/c they were only in 2014)
		
			# Anthem
			rdata[c("27603CA1140005_2012","27603CA1230004_2012","48962CA1210003_2012"),"EXP_PLAN_ID"] <- "27603CA1140007_2014" 
			rdata[c("27603CA1150002_2012","27603CA1240003_2012"),"EXP_PLAN_ID"] <- "27603CA1150020_2014"
			rdata[c("27603CA1160004_2012","27603CA1250012_2012","48962CA1230004_2012"),"EXP_PLAN_ID"] <- "27603CA1160013_2014"
			rdata["27603CA1150001_2012","EXP_PLAN_ID"] <- "27603CA1150012_2014"
			rdata["27603CA1400003_2012","EXP_PLAN_ID"] <- "27603CA1400006_2014"
			rdata["27603CA1420003_2012","EXP_PLAN_ID"] <- "27603CA1420014_2014"
			rdata[c("27603CA1150001_2012","27603CA1150003_2012"),"EXP_PLAN_ID"] <- "27603CA1150004_2014"
			rdata[c("27603CA1400003_2012","27603CA1400002_2012"),"EXP_PLAN_ID"] <- "27603CA1400005_2014"
			rdata[c("27603CA1420003_2012","27603CA1420002_2012"),"EXP_PLAN_ID"] <- "27603CA1420009_2014"
			rdata["27603CA1140002_2012","EXP_PLAN_ID"] <- "27603CA1140004_2014"
			rdata["27603CA1160001_2012","EXP_PLAN_ID"] <- "27603CA1160018_2014"
			rdata["27603CA1190001_2012","EXP_PLAN_ID"] <- "27603CA1190002_2014"
			rdata["27603CA1200001_2012","EXP_PLAN_ID"] <- "27603CA1200002_2014"
			rdata["27603CA1140001_2012","EXP_PLAN_ID"] <- "27603CA1140003_2014"
			rdata["27603CA1160007_2012","EXP_PLAN_ID"] <- "27603CA1160008_2014"
			rdata["27603CA1230003_2012","EXP_PLAN_ID"] <- "27603CA1230018_2014"
			rdata["27603CA1250011_2012","EXP_PLAN_ID"] <- "27603CA1250013_2014"
			rdata["27603CA1240002_2012","EXP_PLAN_ID"] <- "27603CA1150012_2014"
			rdata["48962CA1210002_2012","EXP_PLAN_ID"] <- "27603CA1230018_2014"
			rdata["48962CA1230003_2012","EXP_PLAN_ID"] <- "27603CA1250013_2014"
			rdata[c("27603CA1230002_2012","27603CA1230008_2012","27603CA1230009_2012","48962CA1210004_2012"),"EXP_PLAN_ID"] <- "27603CA1230015_2014"
			rdata[c("27603CA1240001_2012"),"EXP_PLAN_ID"] <- "27603CA1150004_2014"
			rdata[c("27603CA1250006_2012","27603CA1250007_2012","27603CA1250010_2012","48962CA1230001_2012"),"EXP_PLAN_ID"] <- "27603CA1250013_2014"
			rdata[c("27603CA1230005_2012","27603CA1230006_2012","27603CA1230007_2012","27603CA1230010_2012",
				"27603CA1230011_2012","27603CA1260001_2012","48962CA1210001_2012"),"EXP_PLAN_ID"] <- "27603CA1230012_2014"
			rdata[c("27603CA1250001_2012","27603CA1250002_2012","27603CA1250003_2012","27603CA1250004_2012",
				"27603CA1250005_2012","27603CA1270002_2012","48962CA1230002_2012"),"EXP_PLAN_ID"] <- "27603CA1250014_2014"
			rdata[c("27603CA1230001_2012","27603CA1250008_2012"),"EXP_PLAN_ID"] <- "27603CA1111111_2014"
		
			# Blue Shield
			rdata["70285CAMissingG_2012","EXP_PLAN_ID"] <- "70285CA1270001_2014"
			rdata["70285CAMissingSIL_2012","EXP_PLAN_ID"] <- "70285CA1290001_2014"
			rdata["70285CAMissingBR_2012","EXP_PLAN_ID"] <- "70285CA1310001_2014"
			rdata["70285CAMissingSIL2_2012","EXP_PLAN_ID"] <- "70285CA1290001_2014"
		
			# Health Net HMO
			rdata["67138CA0520002_2012","EXP_PLAN_ID"] <- platinum_ids[1]
			rdata["67138CA0520003_2012","EXP_PLAN_ID"] <- gold_ids[1]
			rdata["67138CA0520001_2012","EXP_PLAN_ID"] <- silver_ids[1]
			
			# LA Care
			rdata["92815CA001004_2012","EXP_PLAN_ID"] <- "92815CA0010004_2014"
			rdata["92815CA001005_2012","EXP_PLAN_ID"] <- "92815CA0010005_2014"
			rdata["92815CA001006_2012","EXP_PLAN_ID"] <- "92815CA0010006_2014"
			rdata["92815CA001007_2012","EXP_PLAN_ID"] <- "92815CA0010007_2014"
			rdata["92815CA001008_2012","EXP_PLAN_ID"] <- "92815CA0010008_2014"
		
			# Sharp
			rdata["92499CA0020007_2012","EXP_PLAN_ID"] <- "92499CA0020006_2014" 
			
	# Don't need any plans year 2012 and 2013 that don't have projected member months
			
		#rdata <- rdata[!(rdata$year < 2014 & rdata$PRJ_MM == 0),]
			
	# Don't need any plans year 2017 and 2018 that don't have experience memeber months

		#rdata <- rdata[!(rdata$year > 2016 & rdata$EXP_MM == 0),]
		
	# Now we create a separate object to hold the projections (doing this to be safe!)
	
		fields <- c("EXP_PLAN_ID","PRJ_PLAN_ID","METAL","PLAN_TYPE","COMPANY","EXCHANGE",
			"EXP_MM","EXP_INC_CLM","EXP_REIN","EXP_RSK_ADJ",
			"PRJ_MM","PRJ_INC_CLM","PRJ_REIN","PRJ_RSK_ADJ")
		project <- rdata[rdata$year >= 2014 & !is.na(rdata$PRJ_PLAN_ID),fields]  
		project[,c("PRJ_MM","PRJ_INC_CLM","PRJ_REIN","PRJ_RSK_ADJ","updated")] <- 0
		projection_rows <- rownames(rdata[rdata$year <= 2016 & rdata$PRJ_MM > 0 & !is.na(rdata$EXP_PLAN_ID),]) 
		
		for(j in 1:length(projection_rows)) { 
			plans_to_update <- rownames(project[project$PRJ_PLAN_ID == projection_rows[j],])
		
			if (length(plans_to_update) == 0) {
				project[rdata[projection_rows[j],"EXP_PLAN_ID"],"PRJ_MM"] <- 
					project[rdata[projection_rows[j],"EXP_PLAN_ID"],"PRJ_MM"] + rdata[projection_rows[j],"PRJ_MM"]
				project[rdata[projection_rows[j],"EXP_PLAN_ID"],"PRJ_INC_CLM"] <- 
					project[rdata[projection_rows[j],"EXP_PLAN_ID"],"PRJ_INC_CLM"] + rdata[projection_rows[j],"PRJ_INC_CLM"]
				project[rdata[projection_rows[j],"EXP_PLAN_ID"],"PRJ_RSK_ADJ"] <- 
					project[rdata[projection_rows[j],"EXP_PLAN_ID"],"PRJ_RSK_ADJ"] + rdata[projection_rows[j],"PRJ_RSK_ADJ"]
				project[rdata[projection_rows[j],"EXP_PLAN_ID"],"PRJ_REIN"] <- 
					project[rdata[projection_rows[j],"EXP_PLAN_ID"],"PRJ_REIN"] + rdata[projection_rows[j],"PRJ_REIN"]
			} else if(sum(project[plans_to_update,"EXP_MM"]) > 0) {
				project[plans_to_update,"PRJ_MM"] <- project[plans_to_update,"PRJ_MM"] + 
					project[plans_to_update,"EXP_MM"]/sum(project[plans_to_update,"EXP_MM"]) *
					rdata[projection_rows[j],"PRJ_MM"]
				project[plans_to_update,"PRJ_INC_CLM"] <- project[plans_to_update,"PRJ_INC_CLM"] + 
					project[plans_to_update,"EXP_INC_CLM"]/sum(project[plans_to_update,"EXP_INC_CLM"]) *
					rdata[projection_rows[j],"PRJ_INC_CLM"]
				if (sum(project[plans_to_update,"EXP_RSK_ADJ"]) == 0) {
					project[plans_to_update,"PRJ_RSK_ADJ"] <- project[plans_to_update,"PRJ_RSK_ADJ"] + 
						1/length(plans_to_update) * rdata[projection_rows[j],"PRJ_RSK_ADJ"]
				} else {
					project[plans_to_update,"PRJ_RSK_ADJ"] <- project[plans_to_update,"PRJ_RSK_ADJ"] + 
						project[plans_to_update,"EXP_RSK_ADJ"]/sum(project[plans_to_update,"EXP_RSK_ADJ"]) *
						rdata[projection_rows[j],"PRJ_RSK_ADJ"]
				}
				if (sum(project[plans_to_update,"EXP_REIN"]) == 0) {
					project[plans_to_update,"PRJ_REIN"] <- project[plans_to_update,"PRJ_REIN"] + 
						1/length(plans_to_update) * rdata[projection_rows[j],"PRJ_REIN"]
				} else {
					project[plans_to_update,"PRJ_REIN"] <- project[plans_to_update,"PRJ_REIN"] + 
						project[plans_to_update,"EXP_REIN"]/sum(project[plans_to_update,"EXP_REIN"]) *
						rdata[projection_rows[j],"PRJ_REIN"]
				}	
			} else { # in case plans have zero experience
				project[plans_to_update,"PRJ_MM"] <- project[plans_to_update,"PRJ_MM"] + 
					1/length(plans_to_update) * rdata[projection_rows[j],"PRJ_MM"]
				project[plans_to_update,"PRJ_INC_CLM"] <- project[plans_to_update,"PRJ_INC_CLM"] + 
					1/length(plans_to_update) * rdata[projection_rows[j],"PRJ_INC_CLM"]
				project[plans_to_update,"PRJ_RSK_ADJ"] <- project[plans_to_update,"PRJ_RSK_ADJ"] + 
					1/length(plans_to_update) * rdata[projection_rows[j],"PRJ_RSK_ADJ"]
				project[plans_to_update,"PRJ_REIN"] <- project[plans_to_update,"PRJ_REIN"] + 
					1/length(plans_to_update) * rdata[projection_rows[j],"PRJ_REIN"]
			}
			project[plans_to_update,"updated"] <- project[plans_to_update,"updated"] + 1
		}
		
		import_fields <- c("PRJ_MM","PRJ_INC_CLM","PRJ_REIN","PRJ_RSK_ADJ") 
		rdata[,import_fields] <- 0
		rdata[rownames(project),import_fields] <- project[,import_fields]
		
		# At this point, we have matched all projected members months to a plan except for Cigna, Moda, Contra Costa, and United
		# The unmatched plans account for 0.45% of all projected member months/claims/etc
		# All are small insurers

	# One final issue:
		# Anthem has a catch-all off-exchange PPO experience plan in 2015 with zero projections
		# The metal is "Not Applicable"
		# I'm going to distribute the experience across all Anthem off-exchange PPO plans
		
		plan_to_remove <- rownames(rdata[rdata$METAL == "Not Applicable",])
		plans_to_update <- rownames(rdata[rdata$COMPANY == "Anthem" & rdata$year == 2015 & rdata$EXCHANGE == "No" & rdata$PLAN_TYPE != "HMO",])
		
		rdata[plans_to_update,"EXP_MM"] <- rdata[plans_to_update,"EXP_MM"] + 
			rdata[plans_to_update,"EXP_MM"]/sum(rdata[plans_to_update,"EXP_MM"]) *
			rdata[plan_to_remove,"EXP_MM"]
		rdata[plans_to_update,"EXP_INC_CLM"] <- rdata[plans_to_update,"EXP_INC_CLM"] + 
			rdata[plans_to_update,"EXP_INC_CLM"]/sum(rdata[plans_to_update,"EXP_INC_CLM"]) *
			rdata[plan_to_remove,"EXP_INC_CLM"]
		
		rdata <- rdata[!rdata$METAL == "Not Applicable",]

	# Now we can delete 2012 and 2013 data
	rdata <- rdata[rdata$year >= 2014,]
	
# Verify Risk Adjustment
	# Firms report risk adjustment transfers in rate filing data
	# Actual risk adjustment amounts are reported by CMS (and already populated in MLR data)
	# CMS amounts are correct; scale rate filing data to CMS
	# I'm dropping small insurers that don't make RA transfers 
	# Also adding some that were missing
	# We will flag records if there's no reasonable solution; OK (I think) to use for computing aggregate statistics, 
		# but not for plan-specific statistics/regressions 
	
	
	# Insurer IDs
	ca_insurer_ids <- mlr_data[1:(nrow(mlr_data)-1),c("HIOS_ISSUER_ID","COMPANY_NAME")]
	ca_insurer_ids <- ca_insurer_ids[!duplicated(ca_insurer_ids$HIOS_ISSUER_ID) & !is.na(ca_insurer_ids$HIOS_ISSUER_ID),]
	rownames(ca_insurer_ids) <- ca_insurer_ids$HIOS_ISSUER_ID 
	
	ca_insurers_2014 <- c(setdiff(unique(rdata[rdata$year == 2014,"ISSUER_ID"]),c(71408,23633,37873,10544,64210)),c(99483,64198))
	ca_insurers_2015 <- c(setdiff(unique(rdata[rdata$year == 2015,"ISSUER_ID"]),c(10544)),c(71408,64198,35305))
	ca_insurers_2016 <- c(unique(rdata[rdata$year == 2016,"ISSUER_ID"]),c(37873,40025))
	ca_insurers_2017 <- c(unique(rdata[rdata$year == 2017,"ISSUER_ID"]),c(40025))
	ca_insurers_2018 <- unique(rdata[rdata$year == 2018,"ISSUER_ID"])
	
	# Drop insurers that didn't make risk adjustment transfers
	rdata <- rdata[!(rdata$year == 2014 & !rdata$ISSUER_ID %in% ca_insurers_2014),]
	rdata <- rdata[!(rdata$year == 2015 & !rdata$ISSUER_ID %in% ca_insurers_2015),]
	rdata <- rdata[!(rdata$year == 2016 & !rdata$ISSUER_ID %in% ca_insurers_2016),]
	rdata <- rdata[!(rdata$year == 2017 & !rdata$ISSUER_ID %in% ca_insurers_2017),]
	rdata <- rdata[!(rdata$year == 2018 & !rdata$ISSUER_ID %in% ca_insurers_2018),]
	
	rdata$flagged <- FALSE	
		
		
		
	# Below: I go insurer-by-insurer to resolve any data quality issues that I observed	
		
	# Anthem: 
		# 2014 looks like it's clearly just a minus sign error
		# 2015 and 2016 are pretty far off in aggregate
		
		for(t in 2014:2018) {
			rdata[rdata$year == t & rdata$COMPANY == "Anthem","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Anthem","EXP_RSK_ADJ"] *
					sum(mlr_data[paste("27603_",t,sep=""),"Risk_adjustment_rec"])/
					sum(rdata[rdata$year == t & rdata$COMPANY == "Anthem","EXP_RSK_ADJ"])
		}
	
		# There's one platinum plan with negative claims.  I'm going to take
		# total allowable claims and multiply by the AV 0.9
		
		rdata[rdata$COMPANY == "Anthem" & rdata$year == 2016 & rdata$PLAN_ID == "27603CA1150021","EXP_INC_CLM"] <- 
			0.9 * rdata[rdata$COMPANY == "Anthem" & rdata$year == 2016 & rdata$PLAN_ID == "27603CA1150021","EXP_TAC"]
		
	# Kaiser: very close all years
		
		for(t in 2014:2018) {
			rdata[rdata$year == t & rdata$COMPANY == "Kaiser","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Kaiser","EXP_RSK_ADJ"] *
					sum(mlr_data[paste("40513_",t,sep=""),"Risk_adjustment_rec"])/
					sum(rdata[rdata$year == t & rdata$COMPANY == "Kaiser","EXP_RSK_ADJ"])
		}
	
	# Blue Shield: not bad in 2014, close in all other years
		
		for(t in 2014:2018) {
			rdata[rdata$year == t & rdata$COMPANY == "Blue_Shield","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Blue_Shield","EXP_RSK_ADJ"] *
					sum(mlr_data[paste("70285_",t,sep=""),"Risk_adjustment_rec"])/
					sum(rdata[rdata$year == t & rdata$COMPANY == "Blue_Shield","EXP_RSK_ADJ"])
		}
	
	# Health Net
		
		# Good for 2016-2018, but lots of data issues for 2014 and 2015
		for(t in 2016:2018) {
			rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO","EXP_RSK_ADJ"] *
					sum(mlr_data[paste("99110_",t,sep=""),"Risk_adjustment_rec"])/
					sum(rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO","EXP_RSK_ADJ"])
			rdata[rdata$year == t & rdata$COMPANY == "Health_Net_HMO","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Health_Net_HMO","EXP_RSK_ADJ"] *
					sum(mlr_data[paste("67138_",t,sep=""),"Risk_adjustment_rec"])/
					sum(rdata[rdata$year == t & rdata$COMPANY == "Health_Net_HMO","EXP_RSK_ADJ"])
		}
	
		# Total member months is right, but need to reallocate to the metal tiers for 2014 (use Covered California data)
				# NOTE: this is just to get the right risk-adjusted share (s_av), won't actually use to compute a risk score
			
			tot_hn_ppo_mm_2014 <- sum(rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_PPO","EXP_MM"])
			tot_mm_cat <- tot_hn_ppo_mm_2014 * 0.137813
			tot_mm_bronze <- tot_hn_ppo_mm_2014 * 0.662298
			tot_mm_silver <- tot_hn_ppo_mm_2014 * 0.1542186
			tot_mm_gold <- tot_hn_ppo_mm_2014 * 0.02541014
			tot_mm_platinum <- tot_hn_ppo_mm_2014 * 0.02026027
		
			rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Catastrophic" & rdata$EXP_MM > 0,"EXP_MM"] <- 
				tot_mm_cat/length(rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Catastrophic" & rdata$EXP_MM > 0,"EXP_MM"])
			rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Bronze" & rdata$EXP_MM > 0,"EXP_MM"] <- 
				tot_mm_bronze/length(rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Bronze" & rdata$EXP_MM > 0,"EXP_MM"])
			rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Silver" & rdata$EXP_MM > 0,"EXP_MM"] <- 
				tot_mm_silver/length(rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Silver" & rdata$EXP_MM > 0,"EXP_MM"])
			rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Gold" & rdata$EXP_MM > 0,"EXP_MM"] <- 
				tot_mm_gold/length(rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Gold" & rdata$EXP_MM > 0,"EXP_MM"])
			rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Platinum" & rdata$EXP_MM > 0,"EXP_MM"] <- 
				tot_mm_platinum/length(rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Platinum" & rdata$EXP_MM > 0,"EXP_MM"])
		
			tot_hn_hmo_mm_2014 <- sum(rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_HMO","EXP_MM"])
			tot_mm_silver <- tot_hn_hmo_mm_2014 * 0.865913
			tot_mm_gold <- tot_hn_hmo_mm_2014 * 0.08315124
			tot_mm_platinum <- tot_hn_hmo_mm_2014 * 0.05092285
		
			rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Silver" & rdata$EXP_MM > 0,"EXP_MM"] <- 
				tot_mm_silver/length(rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Silver" & rdata$EXP_MM > 0,"EXP_MM"])
			rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Gold" & rdata$EXP_MM > 0,"EXP_MM"] <- 
				tot_mm_gold/length(rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Gold" & rdata$EXP_MM > 0,"EXP_MM"])
			rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Platinum" & rdata$EXP_MM > 0,"EXP_MM"] <- 
				tot_mm_platinum/length(rdata[rdata$year == 2014 & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Platinum","EXP_MM"])
	
	
		# RA, RI all 0 for 2015 and 2014
			# Solution: Too big of an insurer to just flag
			# Take total RA amount for PPO and HMO in CMS report and divide it using ratios from 2016-2018
		
		sampled_years <- length(c(2016:2018))
		
		cat_PPO_transfer <- sum(rdata[rdata$year >= 2016 & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Catastrophic","EXP_RSK_ADJ"])/sampled_years
		bronze_PPO_transfer <- sum(rdata[rdata$year >= 2016 & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Bronze","EXP_RSK_ADJ"])/sampled_years
		silver_PPO_transfer <- sum(rdata[rdata$year >= 2016 & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Silver","EXP_RSK_ADJ"])/sampled_years
		gold_PPO_transfer <- sum(rdata[rdata$year >= 2016 & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Gold","EXP_RSK_ADJ"])/sampled_years
		platinum_PPO_transfer <- sum(rdata[rdata$year >= 2016 & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Platinum","EXP_RSK_ADJ"])/sampled_years
		total_PPO_transfer <- cat_PPO_transfer + bronze_PPO_transfer + silver_PPO_transfer + gold_PPO_transfer + platinum_PPO_transfer	
		
		silver_HMO_transfer <- sum(rdata[rdata$year >= 2016 & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Silver","EXP_RSK_ADJ"])/sampled_years
		gold_HMO_transfer <- sum(rdata[rdata$year >= 2016 & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Gold","EXP_RSK_ADJ"])/sampled_years
		platinum_HMO_transfer <- sum(rdata[rdata$year >= 2016 & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Platinum","EXP_RSK_ADJ"])/sampled_years
		total_HMO_transfer <- silver_HMO_transfer + gold_HMO_transfer + platinum_HMO_transfer	
		
		for(t in 2014:2015) {
			scaling_PPO_factor <- mlr_data[paste("99110_",t,sep=""),"Risk_adjustment_rec"]/total_PPO_transfer
			scaling_HMO_factor <- mlr_data[paste("67138_",t,sep=""),"Risk_adjustment_rec"]/total_HMO_transfer
			
			# Assign RA transfer by metal and network type 
			rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Catastrophic","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Catastrophic","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Catastrophic","EXP_MM"]) * cat_PPO_transfer * scaling_PPO_factor
			rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Bronze","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Bronze","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Bronze","EXP_MM"]) * bronze_PPO_transfer * scaling_PPO_factor
			rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Silver","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Silver","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Silver","EXP_MM"]) * silver_PPO_transfer * scaling_PPO_factor
			rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Gold","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Gold","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Gold","EXP_MM"]) * gold_PPO_transfer * scaling_PPO_factor
			rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Platinum","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Platinum","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Platinum","EXP_MM"]) * platinum_PPO_transfer * scaling_PPO_factor
			rdata[rdata$year == t & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Silver","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Silver","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Silver","EXP_MM"]) * silver_HMO_transfer * scaling_HMO_factor
			rdata[rdata$year == t & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Gold","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Gold","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Gold","EXP_MM"]) * gold_HMO_transfer * scaling_HMO_factor
			rdata[rdata$year == t & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Platinum","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Platinum","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Platinum","EXP_MM"]) * platinum_HMO_transfer * scaling_HMO_factor
		}
			
	# Chinese Community: aggregate amounts close all years
		
		for(t in 2014:2018) {
			rdata[rdata$year == t & rdata$COMPANY == "Chinese_Community","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Chinese_Community","EXP_RSK_ADJ"] *
					sum(mlr_data[paste("47579_",t,sep=""),"Risk_adjustment_rec"])/
					sum(rdata[rdata$year == t & rdata$COMPANY == "Chinese_Community","EXP_RSK_ADJ"])
		}	
		
	# Western: not great for 2017, but close all other years
		
		for(t in 2014:2018) {
			rdata[rdata$year == t & rdata$COMPANY == "Western","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Western","EXP_RSK_ADJ"] *
					sum(mlr_data[paste("93689_",t,sep=""),"Risk_adjustment_rec"])/
					sum(rdata[rdata$year == t & rdata$COMPANY == "Western","EXP_RSK_ADJ"])
		}	
	
		# RA PMPM same for all plans for 2014-2017
		sampled_years <- length(c(2018))
		cat_transfer <- sum(rdata[rdata$year == 2018 & rdata$COMPANY == "Western" & rdata$METAL == "Catastrophic","EXP_RSK_ADJ"])/sampled_years
		bronze_transfer <- sum(rdata[rdata$year == 2018 & rdata$COMPANY == "Western" & rdata$METAL == "Bronze","EXP_RSK_ADJ"])/sampled_years
		silver_transfer <- sum(rdata[rdata$year == 2018 & rdata$COMPANY == "Western" & rdata$METAL == "Silver","EXP_RSK_ADJ"])/sampled_years
		gold_transfer <- sum(rdata[rdata$year == 2018 & rdata$COMPANY == "Western" & rdata$METAL == "Gold","EXP_RSK_ADJ"])/sampled_years
		platinum_transfer <- sum(rdata[rdata$year == 2018 & rdata$COMPANY == "Western" & rdata$METAL == "Platinum","EXP_RSK_ADJ"])/sampled_years
		total_transfer <- cat_transfer + bronze_transfer + silver_transfer + gold_transfer + platinum_transfer	
		
		for(t in 2015:2017) {
			scaling_factor <- mlr_data[paste("93689_",t,sep=""),"Risk_adjustment_rec"]/total_transfer
			
			# Assign RA transfer by metal
			rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Catastrophic","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Catastrophic","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Catastrophic","EXP_MM"]) * cat_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Bronze","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Bronze","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Bronze","EXP_MM"]) * bronze_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Silver","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Silver","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Silver","EXP_MM"]) * silver_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Gold","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Gold","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Gold","EXP_MM"]) * gold_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Platinum","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Platinum","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Platinum","EXP_MM"]) * platinum_transfer * scaling_factor
		}
		
		# For 2014, I will use ratios from Kaiser b/c the sign of the transfer switched
		
		sampled_years <- length(c(2014))
		cat_transfer <- sum(rdata[rdata$year == 2014 & rdata$COMPANY == "Kaiser" & rdata$METAL == "Catastrophic","EXP_RSK_ADJ"])/sampled_years
		bronze_transfer <- sum(rdata[rdata$year == 2014 & rdata$COMPANY == "Kaiser" & rdata$METAL == "Bronze","EXP_RSK_ADJ"])/sampled_years
		silver_transfer <- sum(rdata[rdata$year == 2014 & rdata$COMPANY == "Kaiser" & rdata$METAL == "Silver","EXP_RSK_ADJ"])/sampled_years
		gold_transfer <- sum(rdata[rdata$year == 2014 & rdata$COMPANY == "Kaiser" & rdata$METAL == "Gold","EXP_RSK_ADJ"])/sampled_years
		platinum_transfer <- sum(rdata[rdata$year == 2014 & rdata$COMPANY == "Kaiser" & rdata$METAL == "Platinum","EXP_RSK_ADJ"])/sampled_years
		total_transfer <- cat_transfer + bronze_transfer + silver_transfer + gold_transfer + platinum_transfer	
		
		for(t in 2014) {
			scaling_factor <- mlr_data[paste("93689_",t,sep=""),"Risk_adjustment_rec"]/total_transfer
			
			# Assign RA transfer by metal
			rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Catastrophic","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Catastrophic","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Catastrophic","EXP_MM"]) * cat_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Bronze","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Bronze","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Bronze","EXP_MM"]) * bronze_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Silver","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Silver","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Silver","EXP_MM"]) * silver_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Gold","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Gold","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Gold","EXP_MM"]) * gold_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Platinum","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Platinum","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Western" & rdata$METAL == "Platinum","EXP_MM"]) * platinum_transfer * scaling_factor
		}
		
		
	# Molina - good all around
		
		for(t in 2014:2018) {
			rdata[rdata$year == t & rdata$COMPANY == "Molina","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Molina","EXP_RSK_ADJ"] *
					sum(mlr_data[paste("18126_",t,sep=""),"Risk_adjustment_rec"])/
					sum(rdata[rdata$year == t & rdata$COMPANY == "Molina","EXP_RSK_ADJ"])
		}	
	
	# LA Care - wrong in 2014, not great in 2016, but close all other years	
		
		for(t in 2015:2018) {
			rdata[rdata$year == t & rdata$COMPANY == "LA_Care","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "LA_Care","EXP_RSK_ADJ"] *
					sum(mlr_data[paste("92815_",t,sep=""),"Risk_adjustment_rec"])/
					sum(rdata[rdata$year == t & rdata$COMPANY == "LA_Care","EXP_RSK_ADJ"])
		}	
		
		# Now fix 2014
		sampled_years <- length(c(2015))
		cat_transfer <- sum(rdata[rdata$year == 2015 & rdata$COMPANY == "LA_Care" & rdata$METAL == "Catastrophic","EXP_RSK_ADJ"])/sampled_years
		bronze_transfer <- sum(rdata[rdata$year == 2015 & rdata$COMPANY == "LA_Care" & rdata$METAL == "Bronze","EXP_RSK_ADJ"])/sampled_years
		silver_transfer <- sum(rdata[rdata$year == 2015 & rdata$COMPANY == "LA_Care" & rdata$METAL == "Silver","EXP_RSK_ADJ"])/sampled_years
		gold_transfer <- sum(rdata[rdata$year == 2015 & rdata$COMPANY == "LA_Care" & rdata$METAL == "Gold","EXP_RSK_ADJ"])/sampled_years
		platinum_transfer <- sum(rdata[rdata$year == 2015 & rdata$COMPANY == "LA_Care" & rdata$METAL == "Platinum","EXP_RSK_ADJ"])/sampled_years
		total_transfer <- cat_transfer + bronze_transfer + silver_transfer + gold_transfer + platinum_transfer	
		
		for(t in 2014) {
			scaling_factor <- mlr_data[paste("92815_",t,sep=""),"Risk_adjustment_rec"]/total_transfer
			
			# Assign RA transfer by metal
			rdata[rdata$year == t & rdata$COMPANY == "LA_Care" & rdata$METAL == "Catastrophic","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "LA_Care" & rdata$METAL == "Catastrophic","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "LA_Care" & rdata$METAL == "Catastrophic","EXP_MM"]) * cat_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "LA_Care" & rdata$METAL == "Bronze","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "LA_Care" & rdata$METAL == "Bronze","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "LA_Care" & rdata$METAL == "Bronze","EXP_MM"]) * bronze_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "LA_Care" & rdata$METAL == "Silver","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "LA_Care" & rdata$METAL == "Silver","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "LA_Care" & rdata$METAL == "Silver","EXP_MM"]) * silver_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "LA_Care" & rdata$METAL == "Gold","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "LA_Care" & rdata$METAL == "Gold","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "LA_Care" & rdata$METAL == "Gold","EXP_MM"]) * gold_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "LA_Care" & rdata$METAL == "Platinum","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "LA_Care" & rdata$METAL == "Platinum","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "LA_Care" & rdata$METAL == "Platinum","EXP_MM"]) * platinum_transfer * scaling_factor
		}
		
		
	# Oscar
		
		for(t in 2016:2018) {
			rdata[rdata$year == t & rdata$COMPANY == "Oscar","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Oscar","EXP_RSK_ADJ"] *
					sum(mlr_data[paste("10544_",t,sep=""),"Risk_adjustment_rec"])/
					sum(rdata[rdata$year == t & rdata$COMPANY == "Oscar","EXP_RSK_ADJ"])
		}	
		
		# Oscar: RA PMPM same for all plans for 2016-2017
		sampled_years <- length(c(2018))
		cat_transfer <- sum(rdata[rdata$year == 2018 & rdata$COMPANY == "Oscar" & rdata$METAL == "Catastrophic","EXP_RSK_ADJ"])/sampled_years
		bronze_transfer <- sum(rdata[rdata$year == 2018 & rdata$COMPANY == "Oscar" & rdata$METAL == "Bronze","EXP_RSK_ADJ"])/sampled_years
		silver_transfer <- sum(rdata[rdata$year == 2018 & rdata$COMPANY == "Oscar" & rdata$METAL == "Silver","EXP_RSK_ADJ"])/sampled_years
		gold_transfer <- sum(rdata[rdata$year == 2018 & rdata$COMPANY == "Oscar" & rdata$METAL == "Gold","EXP_RSK_ADJ"])/sampled_years
		platinum_transfer <- sum(rdata[rdata$year == 2018 & rdata$COMPANY == "Oscar" & rdata$METAL == "Platinum","EXP_RSK_ADJ"])/sampled_years
		total_transfer <- cat_transfer + bronze_transfer + silver_transfer + gold_transfer + platinum_transfer	
		
		for(t in 2016:2017) {
			scaling_factor <- mlr_data[paste("10544_",t,sep=""),"Risk_adjustment_rec"]/total_transfer
			
			# Assign RA transfer by metal
			rdata[rdata$year == t & rdata$COMPANY == "Oscar" & rdata$METAL == "Catastrophic","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Oscar" & rdata$METAL == "Catastrophic","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Oscar" & rdata$METAL == "Catastrophic","EXP_MM"]) * cat_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Oscar" & rdata$METAL == "Bronze","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Oscar" & rdata$METAL == "Bronze","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Oscar" & rdata$METAL == "Bronze","EXP_MM"]) * bronze_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Oscar" & rdata$METAL == "Silver","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Oscar" & rdata$METAL == "Silver","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Oscar" & rdata$METAL == "Silver","EXP_MM"]) * silver_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Oscar" & rdata$METAL == "Gold","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Oscar" & rdata$METAL == "Gold","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Oscar" & rdata$METAL == "Gold","EXP_MM"]) * gold_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Oscar" & rdata$METAL == "Platinum","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Oscar" & rdata$METAL == "Platinum","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Oscar" & rdata$METAL == "Platinum","EXP_MM"]) * platinum_transfer * scaling_factor
		}
			
	# Valley: wrong in 2014, RA PMPM same for all plans for 2014-2017
		
		for(t in 2015:2018) {
			rdata[rdata$year == t & rdata$COMPANY == "Valley","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Valley","EXP_RSK_ADJ"] *
					sum(mlr_data[paste("84014_",t,sep=""),"Risk_adjustment_rec"])/
					sum(rdata[rdata$year == t & rdata$COMPANY == "Valley","EXP_RSK_ADJ"])
		}	
		
		# Now fix 2014-2017
		sampled_years <- length(c(2018))
		cat_transfer <- sum(rdata[rdata$year == 2018 & rdata$COMPANY == "Valley" & rdata$METAL == "Catastrophic","EXP_RSK_ADJ"])/sampled_years
		bronze_transfer <- sum(rdata[rdata$year == 2018 & rdata$COMPANY == "Valley" & rdata$METAL == "Bronze","EXP_RSK_ADJ"])/sampled_years
		silver_transfer <- sum(rdata[rdata$year == 2018 & rdata$COMPANY == "Valley" & rdata$METAL == "Silver","EXP_RSK_ADJ"])/sampled_years
		gold_transfer <- sum(rdata[rdata$year == 2018 & rdata$COMPANY == "Valley" & rdata$METAL == "Gold","EXP_RSK_ADJ"])/sampled_years
		platinum_transfer <- sum(rdata[rdata$year == 2018 & rdata$COMPANY == "Valley" & rdata$METAL == "Platinum","EXP_RSK_ADJ"])/sampled_years
		total_transfer <- cat_transfer + bronze_transfer + silver_transfer + gold_transfer + platinum_transfer	
		
		for(t in 2014:2017) {
			scaling_factor <- mlr_data[paste("84014_",t,sep=""),"Risk_adjustment_rec"]/total_transfer
			
			# Assign RA transfer by metal
			rdata[rdata$year == t & rdata$COMPANY == "Valley" & rdata$METAL == "Catastrophic","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Valley" & rdata$METAL == "Catastrophic","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Valley" & rdata$METAL == "Catastrophic","EXP_MM"]) * cat_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Valley" & rdata$METAL == "Bronze","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Valley" & rdata$METAL == "Bronze","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Valley" & rdata$METAL == "Bronze","EXP_MM"]) * bronze_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Valley" & rdata$METAL == "Silver","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Valley" & rdata$METAL == "Silver","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Valley" & rdata$METAL == "Silver","EXP_MM"]) * silver_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Valley" & rdata$METAL == "Gold","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Valley" & rdata$METAL == "Gold","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Valley" & rdata$METAL == "Gold","EXP_MM"]) * gold_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Valley" & rdata$METAL == "Platinum","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Valley" & rdata$METAL == "Platinum","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Valley" & rdata$METAL == "Platinum","EXP_MM"]) * platinum_transfer * scaling_factor
		} 
			
	# Sharp - I had to have Sam manually add this from the URRT for 2014 and 2015 b/c it was missing in data
	
		# SHARP: RA PMPM same for all plans for 2014 and clearly too low; 
		for(t in 2015:2018) {
			rdata[rdata$year == t & rdata$COMPANY == "Sharp","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Sharp","EXP_RSK_ADJ"] *
					sum(mlr_data[paste("92499_",t,sep=""),"Risk_adjustment_rec"])/
					sum(rdata[rdata$year == t & rdata$COMPANY == "Sharp","EXP_RSK_ADJ"])
		}
		
		# Now fix 2014
		sampled_years <- length(c(2015))
		cat_transfer <- sum(rdata[rdata$year == 2015 & rdata$COMPANY == "Sharp" & rdata$METAL == "Catastrophic","EXP_RSK_ADJ"])/sampled_years
		bronze_transfer <- sum(rdata[rdata$year == 2015 & rdata$COMPANY == "Sharp" & rdata$METAL == "Bronze","EXP_RSK_ADJ"])/sampled_years
		silver_transfer <- sum(rdata[rdata$year == 2015 & rdata$COMPANY == "Sharp" & rdata$METAL == "Silver","EXP_RSK_ADJ"])/sampled_years
		gold_transfer <- sum(rdata[rdata$year == 2015 & rdata$COMPANY == "Sharp" & rdata$METAL == "Gold","EXP_RSK_ADJ"])/sampled_years
		platinum_transfer <- sum(rdata[rdata$year == 2015 & rdata$COMPANY == "Sharp" & rdata$METAL == "Platinum","EXP_RSK_ADJ"])/sampled_years
		total_transfer <- cat_transfer + bronze_transfer + silver_transfer + gold_transfer + platinum_transfer	
		
		for(t in 2014) {
			scaling_factor <- mlr_data[paste("92499_",t,sep=""),"Risk_adjustment_rec"]/total_transfer
			
			# Assign RA transfer by metal
			rdata[rdata$year == t & rdata$COMPANY == "Sharp" & rdata$METAL == "Catastrophic","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Sharp" & rdata$METAL == "Catastrophic","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Sharp" & rdata$METAL == "Catastrophic","EXP_MM"]) * cat_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Sharp" & rdata$METAL == "Bronze","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Sharp" & rdata$METAL == "Bronze","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Sharp" & rdata$METAL == "Bronze","EXP_MM"]) * bronze_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Sharp" & rdata$METAL == "Silver","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Sharp" & rdata$METAL == "Silver","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Sharp" & rdata$METAL == "Silver","EXP_MM"]) * silver_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Sharp" & rdata$METAL == "Gold","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Sharp" & rdata$METAL == "Gold","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Sharp" & rdata$METAL == "Gold","EXP_MM"]) * gold_transfer * scaling_factor
			rdata[rdata$year == t & rdata$COMPANY == "Sharp" & rdata$METAL == "Platinum","EXP_RSK_ADJ"] <- 
				rdata[rdata$year == t & rdata$COMPANY == "Sharp" & rdata$METAL == "Platinum","EXP_MM"]/
				sum(rdata[rdata$year == t & rdata$COMPANY == "Sharp" & rdata$METAL == "Platinum","EXP_MM"]) * platinum_transfer * scaling_factor
		}
		
		# missing total premiums for 2014/2015 - Sam forgot to grab this
		rdata[rdata$COMPANY == "Sharp" & rdata$year == 2014,"EXP_TP"] <- 
			c(3384290,5092104,1478153,2904861,4241536,11701460,7578150,1543852,898765) # from URRT 2016
		rdata[rdata$COMPANY == "Sharp" & rdata$year == 2015,"EXP_TP"] <-
			c(6240918,9725930,4309044,6561434,11146411,27901797,20808595,4774710,1413291) # from URRT 2017
		rdata[rdata$COMPANY == "Sharp" & rdata$year == 2014,"AV_METAL"] <- c(0.9,0.9,0.8,0.8,0.7,0.7,0.6,0.6,0.55)
		rdata[rdata$COMPANY == "Sharp" & rdata$year == 2015,"AV_METAL"] <- c(0.9,0.9,0.8,0.8,0.7,0.7,0.6,0.6,0.55)
		rdata[rdata$COMPANY == "Sharp","PLAN_TYPE"] <- "HMO"
	
		# needs CLAIMS PMPM need to be updated
		rdata[is.na(rdata$EXP_INC_CLM_PMPM),"EXP_INC_CLM_PMPM"] <- 
		rdata[is.na(rdata$EXP_INC_CLM_PMPM),"EXP_INC_CLM"]/
		rdata[is.na(rdata$EXP_INC_CLM_PMPM),"EXP_MM"]
	
	# Missing Insurers: Now I'm going to add the missing insurers from the MLR data:
		# The above covers 11 of the exchange insurers
		# The only other 2 exchange insurers are United and Contra Costa (which were only in for 1 year each)
		# There are some small off-exchange insurers missing as well
		# There are 2 off-exchange insurers (Cigna and Sutter) in the rate filing data - I'm going to delete 
			# and rely on the MLR data for these insurers
		
		# Delete Sutter (2015-2017) and Cigna (present 2014-2015, missing 2016-2017)
		rdata <- rdata[!rdata$COMPANY %in% c("Sutter Health Plus","Cigna"),]
		
		# Add missing insurers (these are all very small and off-exchange except for Contra Costa)
			# Need their total premiums, total member months, RA transfer, and claims
			# Need their total member months
			# Need to assign a metal
			# Classify as exchange or off-exchange
		
		
			# 2014 - missing Contra Costa (99483), Cigna (40025) and Assurant (64198) - use MLR data
			number_adding <- length(c(99483,40025,64198))
			rdata <- rbind(rdata,NA,NA,NA)
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"year"] <- 2014
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"COMPANY"] <- c("Contra_Costa","Cigna","Assurant")
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_MM"] <- c(mlr_data["99483_2014","Member_months"],
				mlr_data["40025_2014","Member_months"],mlr_data["64198_2014","Member_months"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_TP"] <- c(mlr_data["99483_2014","Premiums"],
				mlr_data["40025_2014","Premiums"],mlr_data["64198_2014","Premiums"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_INC_CLM"] <- c(mlr_data["99483_2014","Claims"],
				mlr_data["40025_2014","Claims"],mlr_data["64198_2014","Claims"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_RSK_ADJ"] <- c(mlr_data["99483_2014","Risk_adjustment_rec"],
				mlr_data["40025_2014","Risk_adjustment_rec"],mlr_data["64198_2014","Risk_adjustment_rec"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXCHANGE"] <- c("Yes","No","No")
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"flagged"] <- TRUE
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"METAL"] <- c("Silver","Silver","Platinum")
			
			# 2015 - add Sutter (64210), Cigna (40025), Moda (71408), Time/Assurant (64198), Trustmark (35305) 
			number_adding <- length(c(64210,40025,71408,64198,35305))
			rdata <- rbind(rdata,NA,NA,NA,NA,NA)
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"year"] <- 2015
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"COMPANY"] <- c("Sutter","Cigna","Moda","Assurant","Trustmark")
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_MM"] <- c(mlr_data["64210_2015","Member_months"],
				mlr_data["40025_2015","Member_months"],mlr_data["71408_2015","Member_months"],
				mlr_data["64198_2015","Member_months"],mlr_data["35305_2015","Member_months"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_TP"] <- c(mlr_data["64210_2015","Premiums"],
				mlr_data["40025_2015","Premiums"],mlr_data["71408_2015","Premiums"],
				mlr_data["64198_2015","Premiums"],mlr_data["35305_2015","Premiums"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_INC_CLM"] <- c(mlr_data["64210_2015","Claims"],
				mlr_data["40025_2015","Claims"],mlr_data["71408_2015","Claims"],
				mlr_data["64198_2015","Claims"],mlr_data["35305_2015","Claims"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_RSK_ADJ"] <- c(mlr_data["64210_2015","Risk_adjustment_rec"],
				mlr_data["40025_2015","Risk_adjustment_rec"],mlr_data["71408_2015","Risk_adjustment_rec"],
				mlr_data["64198_2015","Risk_adjustment_rec"],mlr_data["35305_2015","Risk_adjustment_rec"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXCHANGE"] <- "No"
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"flagged"] <- TRUE
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"METAL"] <- c("Silver","Silver","Silver","Platinum","Silver")
			
			# 2016 - United (37873), Sutter (64210) and Cigna (40025) 
			number_adding <- length(c(37873,64210,40025))
			rdata <- rbind(rdata,NA,NA,NA)
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"year"] <- 2016
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"COMPANY"] <- c("United","Sutter","Cigna")
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_MM"] <- c(mlr_data["37873_2016","Member_months"],
				mlr_data["64210_2016","Member_months"],mlr_data["40025_2016","Member_months"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_TP"] <- c(mlr_data["37873_2016","Premiums"],
				mlr_data["64210_2016","Premiums"],mlr_data["40025_2016","Premiums"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_INC_CLM"] <- c(mlr_data["37873_2016","Claims"],
				mlr_data["64210_2016","Claims"],mlr_data["40025_2016","Claims"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_RSK_ADJ"] <- c(mlr_data["37873_2016","Risk_adjustment_rec"],
				mlr_data["64210_2016","Risk_adjustment_rec"],mlr_data["40025_2016","Risk_adjustment_rec"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXCHANGE"] <- c("Yes","No","No")
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"flagged"] <- TRUE
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"METAL"] <- c("Silver","Silver","Silver")
			
			# 2017 - missing Sutter(64210) and Cigna (40025)
			number_adding <- length(c(64210,40025))
			rdata <- rbind(rdata,NA,NA)
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"year"] <- 2017
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"COMPANY"] <- c("Sutter","Cigna")
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_MM"] <- 
				c(mlr_data["64210_2017","Member_months"],mlr_data["40025_2017","Member_months"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_TP"] <- 
				c(mlr_data["64210_2017","Premiums"],mlr_data["40025_2017","Premiums"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_INC_CLM"] <- 
				c(mlr_data["64210_2017","Claims"],mlr_data["40025_2017","Claims"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_RSK_ADJ"] <- 
				c(mlr_data["64210_2017","Risk_adjustment_rec"],mlr_data["40025_2017","Risk_adjustment_rec"])
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXCHANGE"] <- "No"
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"flagged"] <- TRUE
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"METAL"] <- c("Silver","Silver")
			
			# 2018 - missing Sutter(64210)
			number_adding <- length(c(64210))
			rdata <- rbind(rdata,NA)
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"year"] <- 2018
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"COMPANY"] <- c("Sutter")
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_MM"] <- mlr_data["64210_2018","Member_months"]
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_TP"] <- mlr_data["64210_2018","Premiums"]
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_INC_CLM"] <- mlr_data["64210_2018","Claims"]
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXP_RSK_ADJ"] <- mlr_data["64210_2018","Risk_adjustment_rec"]
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"EXCHANGE"] <- "No"
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"flagged"] <- TRUE
			rdata[(nrow(rdata)-number_adding+1):nrow(rdata),"METAL"] <- "Silver"
			
#### Verify Reinsurance	
	
	# Goal: make sure aggregate amounts are the same in the rate filing data
		# as the official numbers reported by CMS-specific
		
	# Some insurers report the same PMPM reinsurance for all plans.  This isn't
		# really a problem as it was for risk adjustment
	
	# 2014
	
		reins_targets_2014 <- mlr_data[paste(ca_insurers_2014,"_2014",sep=""),"Reinsurance_rec"]
		names(reins_targets_2014) <- ca_insurers_2014
		for(f in ca_insurers_2014) {
			current_total <- sum(rdata[rdata$year == 2014 & rdata$ISSUER_ID == f & !is.na(rdata$ISSUER_ID),"EXP_REIN"])
			if(current_total > 0 & !is.na(current_total)) {
				rdata[rdata$year == 2014 & rdata$ISSUER_ID == f & !is.na(rdata$ISSUER_ID),"EXP_REIN"] <- 
					rdata[rdata$year == 2014 & rdata$ISSUER_ID == f & !is.na(rdata$ISSUER_ID),"EXP_REIN"] *
					reins_targets_2014[as.character(f)]/
					sum(rdata[rdata$year == 2014 & rdata$ISSUER_ID == f & !is.na(rdata$ISSUER_ID),"EXP_REIN"])
			}
		}
		
		# Health Net is all zero
		rdata[rdata$year == 2014 & rdata$ISSUER_ID == 99110 & !is.na(rdata$ISSUER_ID),"EXP_REIN"] <- 
			reins_targets_2014[as.character(99110)] * rdata[rdata$year == 2014 & rdata$ISSUER_ID == 99110 & !is.na(rdata$ISSUER_ID),"EXP_MM"]/
				sum(rdata[rdata$year == 2014 & rdata$ISSUER_ID == 99110 & !is.na(rdata$ISSUER_ID),"EXP_MM"])
		rdata[rdata$year == 2014 & rdata$ISSUER_ID == 67138 & !is.na(rdata$ISSUER_ID),"EXP_REIN"] <- 
			reins_targets_2014[as.character(67138)] * rdata[rdata$year == 2014 & rdata$ISSUER_ID == 67138 & !is.na(rdata$ISSUER_ID),"EXP_MM"]/
				sum(rdata[rdata$year == 2014 & rdata$ISSUER_ID == 67138 & !is.na(rdata$ISSUER_ID),"EXP_MM"])
					
		# Contra Costa, Assurant, and Cigna were added from the MLR data
		rdata[rdata$year == 2014 & rdata$COMPANY == "Contra_Costa","EXP_REIN"] <- 
			reins_targets_2014[as.character(99483)] * rdata[rdata$year == 2014 & rdata$COMPANY == "Contra_Costa","EXP_MM"]/
				sum(rdata[rdata$year == 2014 & rdata$COMPANY == "Contra_Costa","EXP_MM"])
		rdata[rdata$year == 2014 & rdata$COMPANY == "Cigna","EXP_REIN"] <- 
			reins_targets_2014[as.character(40025)] * rdata[rdata$year == 2014 & rdata$COMPANY == "Cigna","EXP_MM"]/
				sum(rdata[rdata$year == 2014 & rdata$COMPANY == "Cigna","EXP_MM"])
		rdata[rdata$year == 2014 & rdata$COMPANY == "Assurant","EXP_REIN"] <- 
			reins_targets_2014[as.character(64198)] * rdata[rdata$year == 2014 & rdata$COMPANY == "Assurant","EXP_MM"]/
				sum(rdata[rdata$year == 2014 & rdata$COMPANY == "Assurant","EXP_MM"])
		
	# 2015
	
		reins_targets_2015 <- mlr_data[paste(ca_insurers_2015,"_2015",sep=""),"Reinsurance_rec"]
		names(reins_targets_2015) <- ca_insurers_2015
		for(f in ca_insurers_2015) {
			current_total <- sum(rdata[rdata$year == 2015 & rdata$ISSUER_ID == f & !is.na(rdata$ISSUER_ID),"EXP_REIN"])
			if(current_total > 0 & !is.na(current_total)) {
				rdata[rdata$year == 2015 & rdata$ISSUER_ID == f & !is.na(rdata$ISSUER_ID),"EXP_REIN"] <- 
					rdata[rdata$year == 2015 & rdata$ISSUER_ID == f & !is.na(rdata$ISSUER_ID),"EXP_REIN"] *
					reins_targets_2015[as.character(f)]/
					sum(rdata[rdata$year == 2015 & rdata$ISSUER_ID == f & !is.na(rdata$ISSUER_ID),"EXP_REIN"])
			}
		}
		
		# Health Net is all zero
		rdata[rdata$year == 2015 & rdata$ISSUER_ID == 99110 & !is.na(rdata$ISSUER_ID),"EXP_REIN"] <- 
			reins_targets_2015[as.character(99110)] * rdata[rdata$year == 2015 & rdata$ISSUER_ID == 99110 & !is.na(rdata$ISSUER_ID),"EXP_MM"]/
				sum(rdata[rdata$year == 2015 & rdata$ISSUER_ID == 99110 & !is.na(rdata$ISSUER_ID),"EXP_MM"])
		rdata[rdata$year == 2015 & rdata$ISSUER_ID == 67138 & !is.na(rdata$ISSUER_ID),"EXP_REIN"] <- 
			reins_targets_2015[as.character(67138)] * rdata[rdata$year == 2015 & rdata$ISSUER_ID == 67138 & !is.na(rdata$ISSUER_ID),"EXP_MM"]/
				sum(rdata[rdata$year == 2015 & rdata$ISSUER_ID == 67138 & !is.na(rdata$ISSUER_ID),"EXP_MM"])
			
		
		# Assurant     Cigna      Moda    Sutter Trustmark were added from the MLR data
		
		rdata[rdata$year == 2015 & rdata$COMPANY == "Cigna","EXP_REIN"] <- 
			reins_targets_2015[as.character(40025)] * rdata[rdata$year == 2015 & rdata$COMPANY == "Cigna","EXP_MM"]/
				sum(rdata[rdata$year == 2015 & rdata$COMPANY == "Cigna","EXP_MM"])
		rdata[rdata$year == 2015 & rdata$COMPANY == "Assurant","EXP_REIN"] <- 
			reins_targets_2015[as.character(64198)] * rdata[rdata$year == 2015 & rdata$COMPANY == "Assurant","EXP_MM"]/
				sum(rdata[rdata$year == 2015 & rdata$COMPANY == "Assurant","EXP_MM"])
		rdata[rdata$year == 2015 & rdata$COMPANY == "Moda","EXP_REIN"] <- 
			reins_targets_2015[as.character(71408)] * rdata[rdata$year == 2015 & rdata$COMPANY == "Moda","EXP_MM"]/
				sum(rdata[rdata$year == 2015 & rdata$COMPANY == "Moda","EXP_MM"])
		rdata[rdata$year == 2015 & rdata$COMPANY == "Sutter","EXP_REIN"] <- 
			reins_targets_2015[as.character(64210)] * rdata[rdata$year == 2015 & rdata$COMPANY == "Sutter","EXP_MM"]/
				sum(rdata[rdata$year == 2015 & rdata$COMPANY == "Sutter","EXP_MM"])
		rdata[rdata$year == 2015 & rdata$COMPANY == "Trustmark","EXP_REIN"] <- 
			reins_targets_2015[as.character(35305)] * rdata[rdata$year == 2015 & rdata$COMPANY == "Trustmark","EXP_MM"]/
				sum(rdata[rdata$year == 2015 & rdata$COMPANY == "Trustmark","EXP_MM"])
		
	# 2016
	
		reins_targets_2016 <- mlr_data[paste(ca_insurers_2016,"_2016",sep=""),"Reinsurance_rec"]
		names(reins_targets_2016) <- ca_insurers_2016
		for(f in ca_insurers_2016) {
			current_total <- sum(rdata[rdata$year == 2016 & rdata$ISSUER_ID == f & !is.na(rdata$ISSUER_ID),"EXP_REIN"])
			if(current_total > 0 & !is.na(current_total)) {
				rdata[rdata$year == 2016 & rdata$ISSUER_ID == f & !is.na(rdata$ISSUER_ID),"EXP_REIN"] <- 
					rdata[rdata$year == 2016 & rdata$ISSUER_ID == f & !is.na(rdata$ISSUER_ID),"EXP_REIN"] *
					reins_targets_2016[as.character(f)]/
					sum(rdata[rdata$year == 2016 & rdata$ISSUER_ID == f & !is.na(rdata$ISSUER_ID),"EXP_REIN"])
			}
		}
		
		# Cigna Sutter United were added from MLR Data
		rdata[rdata$year == 2016 & rdata$COMPANY == "Cigna","EXP_REIN"] <- 
			reins_targets_2016[as.character(40025)] * rdata[rdata$year == 2016 & rdata$COMPANY == "Cigna","EXP_MM"]/
				sum(rdata[rdata$year == 2016 & rdata$COMPANY == "Cigna","EXP_MM"])
		rdata[rdata$year == 2016 & rdata$COMPANY == "Sutter","EXP_REIN"] <- 
			reins_targets_2016[as.character(64210)] * rdata[rdata$year == 2016 & rdata$COMPANY == "Sutter","EXP_MM"]/
				sum(rdata[rdata$year == 2016 & rdata$COMPANY == "Sutter","EXP_MM"])
		rdata[rdata$year == 2016 & rdata$COMPANY == "United","EXP_REIN"] <- 
			reins_targets_2016[as.character(37873)] * rdata[rdata$year == 2016 & rdata$COMPANY == "United","EXP_MM"]/
				sum(rdata[rdata$year == 2016 & rdata$COMPANY == "United","EXP_MM"])
		
	# Create RA and RI PMPM
	rdata$EXP_RSK_ADJ_PMPM <- rdata$EXP_RSK_ADJ/rdata$EXP_MM
	rdata$EXP_REIN_PMPM <- rdata$EXP_REIN/rdata$EXP_MM
	rdata$EXP_TP_PMPM <- rdata$EXP_TP/rdata$EXP_MM

	rdata[is.na(rdata$EXP_RSK_ADJ_PMPM),"EXP_RSK_ADJ_PMPM"] <- 0
	rdata[is.na(rdata$EXP_REIN_PMPM),"EXP_REIN_PMPM"] <- 0
	rdata[is.na(rdata$EXP_TP_PMPM),"EXP_TP_PMPM"] <- 0

# ISSUES:
	# 1) Plans w/ zero member months - causes RA and RI PMPM amounts to be NA
	# 2) Plans w/ zero claims or zero projected claims
	# 3) Off exchange plans
	
	# Drop plans with no useful information
	#rdata <- rdata[!(rdata$EXP_MM == 0 & rdata$PRJ_MM == 0 & rdata$EXP_INC_CLM == 0),] 
	
# Create cost targets and firm level statistics (e.g., admin costs)	

	fields <- c("COMPANY_NAME","HIOS","Year","Member_months","Premiums","Claims","RA_transfer","Reinsurance",
		"Proj_Average_premiums","Proj_Average_claims","Proj_Average_risk_adj","Proj_reins_factor")

	# Create insurer and year variables
	rdata$insurer_small <- "Small_Insurer"
	rdata[rdata$COMPANY == "Anthem","insurer_small"] <- "Anthem"
	rdata[rdata$COMPANY == "Blue_Shield","insurer_small"] <- "Blue_Shield"
	rdata[rdata$COMPANY %in% c("Health_Net_PPO","Health_Net_HMO"),"insurer_small"] <- "Health_Net"
	rdata[rdata$COMPANY == "Kaiser","insurer_small"] <- "Kaiser"
	rdata$insurer_small_year <- paste(rdata$insurer_small,rdata$year,sep="_")
	# No create convergence output with all plans

	conv_output <- rdata[!duplicated(rdata$insurer_small_year),c("insurer_small","ISSUER_ID","year")]
	conv_output <- conv_output[order(conv_output$insurer_small,conv_output$year),]
	rownames(conv_output) <- paste(conv_output$insurer_small,conv_output$year,sep="_")

	conv_output$Member_months <- by(rdata$EXP_MM,rdata$insurer_small_year,sum)[rownames(conv_output)]
	conv_output$Premiums <- by(rdata$EXP_TP,rdata$insurer_small_year,sum)[rownames(conv_output)]
	conv_output$Claims <- by(rdata$EXP_INC_CLM,rdata$insurer_small_year,sum)[rownames(conv_output)]
	conv_output$RA <- by(rdata$EXP_RSK_ADJ,rdata$insurer_small_year,sum)[rownames(conv_output)]
	conv_output$RI <- by(rdata$EXP_REIN,rdata$insurer_small_year,sum,na.rm=T)[rownames(conv_output)]
	
	# There are some issues with small insurers, but we don't care about them for projection purposes
	conv_output$Proj_member_months <- by(rdata$PRJ_MM,rdata$insurer_small_year,sum)[rownames(conv_output)]
	conv_output$Proj_Premiums <- by(rdata$PRJ_TP,rdata$insurer_small_year,sum)[rownames(conv_output)]
	conv_output$Proj_Claims <- by(rdata$PRJ_INC_CLM,rdata$insurer_small_year,sum)[rownames(conv_output)]
	conv_output$Proj_RA <- by(rdata$PRJ_RSK_ADJ,rdata$insurer_small_year,sum)[rownames(conv_output)]
	conv_output$Proj_RI <- by(rdata$PRJ_REIN,rdata$insurer_small_year,sum)[rownames(conv_output)]
	
	
		# Sam didn't grab Blue Shield's 2014 projected total premium, so adding here from the URRT 2014
		#conv_output["Blue_Shield_2014","Proj_Premiums"] <- 1138813232
	
	conv_output$Avg_Prem <- conv_output$Premiums/conv_output$Member_months
	conv_output$Avg_Claims <- conv_output$Claims/conv_output$Member_months
	conv_output$Avg_RA <- conv_output$RA/conv_output$Member_months
	conv_output$Avg_RI <- conv_output$RI/conv_output$Member_months
	conv_output$Avg_Proj_Prem <- conv_output$Proj_Premiums/conv_output$Proj_member_months
	conv_output$Avg_Proj_Claims <- conv_output$Proj_Claims/conv_output$Proj_member_months
	conv_output$Avg_Proj_RA <- conv_output$Proj_RA/conv_output$Proj_member_months
	conv_output$Avg_Proj_RI <- conv_output$Proj_RI/conv_output$Proj_member_months

	conv_output$Predicted_cost <- conv_output$Avg_Proj_Claims - conv_output$Avg_Proj_RA - conv_output$Avg_Proj_RI 
	conv_output$Actual_cost <- conv_output$Avg_Claims - conv_output$Avg_RA - conv_output$Avg_RI 
	conv_output$Prediction_error <- conv_output$Predicted_cost - conv_output$Actual_cost
	
	write.csv(conv_output,"data/final/convergence.csv")
	
	
##### Enumerate all plans and Identify Missing Plans in Rate Data
	# We have data on all plan-years, but not every plan-market-year
	
	
	# Create object with all plan-market-years
	plan_data <- read.csv("data/plan_data.csv") # Covered California plan data by year and rating area
	plan_data$Plan_ID_Order <- as.character(plan_data$Plan_ID_Order)
	rownames(plan_data) <- paste(plan_data$Plan_Name,plan_data$ENROLLMENT_YEAR,
		plan_data$region,sep="")
	plan_data <- plan_data[plan_data$Plan_ID_Order,]
	
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
	plan_data$Plan_Name_Small_NOHSACAT <- as.character(plan_data$Plan_Name_Small_NOHSACAT)
	plan_data$insurer_small <- plan_data$Insurer
	plan_data[!plan_data$insurer_small %in% c("Anthem","Blue_Shield","Health_Net","Kaiser"),"insurer_small"] <- "Small_Insurer"
	
	keep_fields <- c("pmt_name","Plan_Name_Small_NOHSACAT","ENROLLMENT_YEAR","region","Metal_Level","PLAN_NETWORK_TYPE","insurer_small")
	plan_data$pmt_name <- paste(plan_data$Plan_Name_Small_NOHSACAT,plan_data$ENROLLMENT_YEAR,plan_data$region,sep="")
	plans_pmt <- plan_data[!duplicated(plan_data$pmt_name),keep_fields]
	rownames(plans_pmt) <- plans_pmt$pmt_name
	
	plans_pmt$plan_name <- plans_pmt$Plan_Name_Small_NOHSACAT
	plans_pmt$year <- plans_pmt$ENROLLMENT_YEAR
	plans_pmt$rating_area <- plans_pmt$region
	plans_pmt$HMO <- as.numeric(plans_pmt$PLAN_NETWORK_TYPE == "HMO")
	plans_pmt$Insurer <- plans_pmt$insurer_small
	
	keep_fields <- c("pmt_name","plan_name","year","rating_area","Metal_Level","HMO","Insurer")
	plans_pmt <- plans_pmt[,keep_fields]
	
	# Combine catastrophic and bronze
	rdata[rdata$METAL == "Catastrophic","METAL"] <- "Bronze"
	
	# Now create the plan_name variable in rdata object
	
		rdata$plan_name <- NA
	
		# Initialize all plans to Small by metal (note that I am going to combine bronze and catastrophic)
		rdata[,"plan_name"] <- "SMALL_BR"
		rdata[rdata$METAL == "Silver","plan_name"] <- "SMALL_SIL"
		rdata[rdata$METAL == "Gold","plan_name"] <- "SMALL_G"
		rdata[rdata$METAL == "Platinum","plan_name"] <- "SMALL_P"
	
		# Label Kaiser plans
		rdata[rdata$COMPANY == "Kaiser","plan_name"] <- "KA_BR"
		rdata[rdata$COMPANY == "Kaiser" & rdata$METAL == "Silver","plan_name"] <- "KA_SIL"
		rdata[rdata$COMPANY == "Kaiser" & rdata$METAL == "Gold","plan_name"] <- "KA_G"
		rdata[rdata$COMPANY == "Kaiser" & rdata$METAL == "Platinum","plan_name"] <- "KA_P"
	
		# Label Anthem Plans
		rdata[rdata$COMPANY == "Anthem","plan_name"] <- "ANT_BR"
		rdata[rdata$COMPANY == "Anthem" & rdata$METAL == "Silver","plan_name"] <- "ANT_SIL"
		rdata[rdata$COMPANY == "Anthem" & rdata$METAL == "Gold","plan_name"] <- "ANT_G"
		rdata[rdata$COMPANY == "Anthem" & rdata$METAL == "Platinum","plan_name"] <- "ANT_P"
		rdata[rdata$COMPANY == "Anthem" & rdata$METAL == "Silver" & rdata$PLAN_TYPE == "HMO","plan_name"] <- "ANT_SIL3"
		rdata[rdata$COMPANY == "Anthem" & rdata$METAL == "Gold"& rdata$PLAN_TYPE == "HMO","plan_name"] <- "ANT_G3"
		rdata[rdata$COMPANY == "Anthem" & rdata$METAL == "Platinum" & rdata$PLAN_TYPE == "HMO","plan_name"] <- "ANT_P3"
		
		# Label Blue Shield Plans
		rdata[rdata$COMPANY == "Blue_Shield","plan_name"] <- "BS_BR"
		rdata[rdata$COMPANY == "Blue_Shield" & rdata$METAL == "Silver","plan_name"] <- "BS_SIL"
		rdata[rdata$COMPANY == "Blue_Shield" & rdata$METAL == "Gold","plan_name"] <- "BS_G"
		rdata[rdata$COMPANY == "Blue_Shield" & rdata$METAL == "Platinum","plan_name"] <- "BS_P"
		rdata[rdata$COMPANY == "Blue_Shield" & rdata$METAL == "Silver" & rdata$PLAN_TYPE == "HMO","plan_name"] <- "BS_SIL3"
		rdata[rdata$COMPANY == "Blue_Shield" & rdata$METAL == "Gold" & rdata$PLAN_TYPE == "HMO","plan_name"] <- "BS_G3"
		rdata[rdata$COMPANY == "Blue_Shield" & rdata$METAL == "Platinum" & rdata$PLAN_TYPE == "HMO","plan_name"] <- "BS_P3"
		
			# Blue Shield didn't have HMO plans in 2015 or 2016, but there are some in the rate filing data
			# These plans have 0 experience, bet have a very small number of positive projected member months
			# Combine with PPO
			rdata[rdata$COMPANY == "Blue_Shield" & rdata$METAL == "Silver" & rdata$year %in% c(2015,2016),"plan_name"] <- "BS_SIL"
			rdata[rdata$COMPANY == "Blue_Shield" & rdata$METAL == "Gold" & rdata$year %in% c(2015,2016),"plan_name"] <- "BS_G"
			rdata[rdata$COMPANY == "Blue_Shield" & rdata$METAL == "Platinum" & rdata$year %in% c(2015,2016),"plan_name"] <- "BS_P"
			
			# Blue Shield didn't offer HMO plans in rating area 11 in 2018, but there are some in rate filing data
			# These plans have 0 experience, but have a minute number of positive projected member months 
			# Combine with PPO
			rdata[rdata$COMPANY == "Blue_Shield" & rdata$METAL == "Silver" & 
				rdata$year %in% c(2018) & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8060011","70285CA8060070"),"plan_name"] <- "BS_SIL"
			rdata[rdata$COMPANY == "Blue_Shield" & rdata$METAL == "Gold" & 
				rdata$year %in% c(2018) & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040011"),"plan_name"] <- "BS_G"
			rdata[rdata$COMPANY == "Blue_Shield" & rdata$METAL == "Platinum" & 
				rdata$year %in% c(2018) & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8050011"),"plan_name"] <- "BS_P"
		
		# Label Health Net Plans
		rdata[rdata$COMPANY == "Health_Net_PPO","plan_name"] <- "HN_BR"
		rdata[rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Silver","plan_name"] <- "HN_SIL"
		rdata[rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Gold","plan_name"] <- "HN_G"
		rdata[rdata$COMPANY == "Health_Net_PPO" & rdata$METAL == "Platinum","plan_name"] <- "HN_P"
		rdata[rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Bronze","plan_name"] <- "HN_BR3"
		rdata[rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Silver","plan_name"] <- "HN_SIL3"
		rdata[rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Gold","plan_name"] <- "HN_G3"
		rdata[rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Platinum","plan_name"] <- "HN_P3"
		
			# HSPs will be classifed as non-HMO
			rdata[rdata$PLAN_ID == "67138CA0630001" & !is.na(rdata$PLAN_ID),"plan_name"] <- "HN_BR"
			rdata[rdata$PLAN_ID == "67138CA0630002" & !is.na(rdata$PLAN_ID),"plan_name"] <- "HN_BR"
			rdata[rdata$PLAN_ID == "67138CA0630003" & !is.na(rdata$PLAN_ID),"plan_name"] <- "HN_P"
			rdata[rdata$PLAN_ID == "67138CA0630004" & !is.na(rdata$PLAN_ID),"plan_name"] <- "HN_G"			
			rdata[rdata$PLAN_ID == "67138CA0630005" & !is.na(rdata$PLAN_ID),"plan_name"] <- "HN_SIL"

			# Health didn't offer HMO plans in rating area 14 in 2014 and 2015, but there are some in rate filings
			# These plans have 0 experience, but have positive projected member months 
			# Combine with PPO
			rdata[rdata$year < 2016 & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Silver" & !is.na(rdata$PLAN_ID) &
				rdata$PLAN_ID %in% c("67138CA0520019","67138CA0520020","67138CA0520021","67138CA0620019","67138CA0620020","67138CA0620021"),"plan_name"] <- "HN_SIL"
			rdata[rdata$year < 2016 & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Gold" & !is.na(rdata$PLAN_ID) &
				rdata$PLAN_ID %in% c("67138CA0520019","67138CA0520020","67138CA0520021","67138CA0620019","67138CA0620020","67138CA0620021"),"plan_name"] <- "HN_G"
			rdata[rdata$year < 2016 & rdata$COMPANY == "Health_Net_HMO" & rdata$METAL == "Platinum" & !is.na(rdata$PLAN_ID) &
				rdata$PLAN_ID %in% c("67138CA0520019","67138CA0520020","67138CA0520021","67138CA0620019","67138CA0620020","67138CA0620021"),"plan_name"] <- "HN_P"
			
			# Health Net didn't offer PPO plans in rating area 14 in 2016-2018, but there are some in rate filings
			# These plans have 5 member months and 7 projected member months 
			# Combine with HMO
			rdata[!is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("99110CA0350073") & rdata$year >= 2016,"plan_name"] <- "HN_SIL3"
			rdata[!is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("99110CA0350021") & rdata$year >= 2016,"plan_name"] <- "HN_G3"
			rdata[!is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("99110CA0350013") & rdata$year >= 2016,"plan_name"] <- "HN_P3"
			
			# Health Net didn't offer silver/gold/platinum PPO plans in rating areas 15-19 in 2014-2017, but there are some in rate filings
			# All these plans combined have 1723 member months (~140 people); projected member months is 705,000
			# Combine with HMO
			
			relevant_plan_ids <- c("99110CA0350046","99110CA0350047","99110CA0350048","99110CA0350049","99110CA0350050","99110CA0350051","99110CA0350052",
				"99110CA0350053","99110CA0350074","99110CA0350075","99110CA0350076","99110CA0350077","99110CA0400006","99110CA0400007","99110CA0400008",
				"99110CA0400009","99110CA0400010","99110CA0400016","99110CA0400017","99110CA0400018","99110CA0400019","99110CA0400020","99110CA0400026",
				"99110CA0400027","99110CA0400028","99110CA0400029","99110CA0400030","99110CA0400052","99110CA0400053","99110CA0400054","99110CA0400055",
				"99110CA0400056","99110CA0400058","99110CA0400059","99110CA0400060","99110CA0400061","99110CA0400062","99110CA0400064","99110CA0400065",
				"99110CA0400066","99110CA0400067","99110CA0400068")
			
			rdata[rdata$METAL == "Silver" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% relevant_plan_ids & 
				rdata$year %in% c(2014:2017),"plan_name"] <- "HN_SIL3"
			rdata[rdata$METAL == "Gold" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% relevant_plan_ids & 
				rdata$year %in% c(2014:2017),"plan_name"] <- "HN_G3"
			rdata[rdata$METAL == "Platinum" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% relevant_plan_ids & 
				rdata$year %in% c(2014:2017),"plan_name"] <- "HN_P3"
		
		
		
		rdata[rdata$COMPANY %in% c("Health_Net_HMO","Health_Net_PPO"),"COMPANY"] <- "Health_Net"

	# Now we create the region variable in the rdata object
		# For many plans, we don't know region, so we'll assign 0.
		# Don't know Kaiser, LA Care, United, Oscar, Molina

		rdata$region <- 0

		# Anthem
		rdata[rdata$COMPANY == "Anthem" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("27603CA1150004","27603CA1150012","27603CA1150020","27603CA1240001"),"region"] <- 3
		rdata[rdata$COMPANY == "Anthem" & !is.na(rdata$PLAN_ID) & 
			rdata$PLAN_ID %in% c("27603CA1160008","27603CA1160013","27603CA1160018","27603CA1200002","27603CA1420009","27603CA1420014"),"region"] <- 4
		rdata[rdata$COMPANY == "Anthem" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("27603CA1150005","27603CA1150013","27603CA1150021","27603CA1240002"),"region"] <- 7
		rdata[rdata$COMPANY == "Anthem" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("27603CA1150006","27603CA1150014","27603CA1150022","27603CA1240003"),"region"] <- 11
		rdata[rdata$COMPANY == "Anthem" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("27603CA1150007","27603CA1150015","27603CA1150023",
			"27603CA1160009","27603CA1160014","27603CA1160019","27603CA1200003","27603CA1420010","27603CA1420015","27603CA1240004"),"region"] <- 15
		rdata[rdata$COMPANY == "Anthem" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("27603CA1150008","27603CA1150016","27603CA1150024",
			"27603CA1160010","27603CA1160015","27603CA1160020","27603CA1200004","27603CA1420011","27603CA1420016","27603CA1240005"),"region"] <- 16
		rdata[rdata$COMPANY == "Anthem" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("27603CA1150009","27603CA1150017","27603CA1150025","27603CA1240006"),"region"] <- 17
		rdata[rdata$COMPANY == "Anthem" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("27603CA1150010","27603CA1150018","27603CA1150026",
			"27603CA1160011","27603CA1160016","27603CA1160021","27603CA1200005","27603CA1420012","27603CA1420017","27603CA1240007"),"region"] <- 18
		rdata[rdata$COMPANY == "Anthem" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("27603CA1150011","27603CA1150019","27603CA1150027",
			"27603CA1160012","27603CA1160017","27603CA1160022","27603CA1200006","27603CA1420013","27603CA1420018","27603CA1240008"),"region"] <- 19
	
		# Blue Shield
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040001","70285CA8050001","70285CA8060001","70285CA8060053"),"region"] <- 1
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040002","70285CA8050002","70285CA8060002","70285CA8060054"),"region"] <- 2
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040003","70285CA8050003","70285CA8060003","70285CA8060055"),"region"] <- 3
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040004","70285CA8050004","70285CA8060004","70285CA8060056"),"region"] <- 4
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040005","70285CA8050005","70285CA8060005","70285CA8060057"),"region"] <- 5
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040006","70285CA8050006","70285CA8060006","70285CA8060058"),"region"] <- 6
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040007","70285CA8050007","70285CA8060007","70285CA8060059"),"region"] <- 7
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040008","70285CA8050008","70285CA8060008","70285CA8060060"),"region"] <- 8
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040009","70285CA8050009","70285CA8060009","70285CA8060061"),"region"] <- 9
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040010","70285CA8050010","70285CA8060010","70285CA8060062"),"region"] <- 10
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040011","70285CA8050011","70285CA8060011","70285CA8060070"),"region"] <- 11
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040012","70285CA8050012","70285CA8060012","70285CA8060063"),"region"] <- 12
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040013","70285CA8050013","70285CA8060013"),"region"] <- 13
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040014","70285CA8050014","70285CA8060014","70285CA8060064"),"region"] <- 14
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040015","70285CA8050015","70285CA8060015","70285CA8060065"),"region"] <- 15
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040016","70285CA8050016","70285CA8060016","70285CA8060066"),"region"] <- 16
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040017","70285CA8050017","70285CA8060017","70285CA8060067"),"region"] <- 17
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040018","70285CA8050018","70285CA8060018","70285CA8060068"),"region"] <- 18
		rdata[rdata$COMPANY == "Blue_Shield" & !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040019","70285CA8050019","70285CA8060019","70285CA8060069"),"region"] <- 19

			# Region should be 0 for HMO plans in 2015 and 2016
			rdata[rdata$COMPANY == "Blue_Shield" & rdata$year %in% c(2015,2016) & rdata$PLAN_TYPE == "HMO","region"] <- 0
			rdata[rdata$COMPANY == "Blue_Shield" & rdata$year %in% c(2015,2016) & rdata$PLAN_TYPE == "HMO","PLAN_TYPE"] <- "PPO"
			
			# Blue Shield didn't offer HMO plans in rating area 11 in 2018
			rdata[rdata$COMPANY == "Blue_Shield" & rdata$year %in% c(2018) & rdata$PLAN_TYPE == "HMO" & rdata$region == 11,"region"] <- 0
			rdata[rdata$COMPANY == "Blue_Shield" & rdata$year %in% c(2018) & rdata$PLAN_TYPE == "HMO" & rdata$region == 11,"PLAN_TYPE"] <- "PPO"
			
			# Have to reassign projections for gold, 2017, HMO, region 2 b/c it had 0 member months (assign to region 4)
				# These projections are extremely small and will make hardly any difference
			rdata[rdata$COMPANY == "Blue_Shield" & rdata$year %in% c(2017) & 
				 !is.na(rdata$PLAN_ID) & rdata$PLAN_ID %in% c("70285CA8040002") & rdata$region == 2,"region"] <- 4
			
		# Health Net
		rdata[rdata$COMPANY == "Health_Net" & !is.na(rdata$PLAN_ID) & 
			rdata$PLAN_ID %in% c("99110CA0340006","99110CA0340012","99110CA0340018","99110CA0340024","99110CA0340030","99110CA0350006","99110CA0350014","99110CA0350066"),"region"] <- 2
		rdata[rdata$COMPANY == "Health_Net" & !is.na(rdata$PLAN_ID) & 
			rdata$PLAN_ID %in% c("99110CA0400003","99110CA0400013","99110CA0400023","99110CA0400033","99110CA0400043","99110CA0400051","99110CA0400057","99110CA0400063"),"region"] <- 3
		rdata[rdata$COMPANY == "Health_Net" & !is.na(rdata$PLAN_ID) & 
			rdata$PLAN_ID %in% c("99110CA0340007","99110CA0340013","99110CA0340019","99110CA0340025","99110CA0340031","99110CA0350007","99110CA0350015","99110CA0350067"),"region"] <- 4
		rdata[rdata$COMPANY == "Health_Net" & !is.na(rdata$PLAN_ID) & 
			rdata$PLAN_ID %in% c("99110CA0340008","99110CA0340014","99110CA0340020","99110CA0340026","99110CA0340032","99110CA0350008","99110CA0350016","99110CA0350068"),"region"] <- 5
		rdata[rdata$COMPANY == "Health_Net" & !is.na(rdata$PLAN_ID) & 
			rdata$PLAN_ID %in% c("99110CA0350009","99110CA0350017","99110CA0350069"),"region"] <- 7
		rdata[rdata$COMPANY == "Health_Net" & !is.na(rdata$PLAN_ID) & 
			rdata$PLAN_ID %in% c("99110CA0340009","99110CA0340015","99110CA0340021","99110CA0340027","99110CA0340033","99110CA0350010","99110CA0350018","99110CA0350070"),"region"] <- 8
		rdata[rdata$COMPANY == "Health_Net" & !is.na(rdata$PLAN_ID) & 
			rdata$PLAN_ID %in% c("99110CA0340010","99110CA0340016","99110CA0340022","99110CA0340028","99110CA0340034","99110CA0350011","99110CA0350019","99110CA0350071"),"region"] <- 9
		rdata[rdata$COMPANY == "Health_Net" & !is.na(rdata$PLAN_ID) & 
			rdata$PLAN_ID %in% c("99110CA0340011","99110CA0340017","99110CA0340023","99110CA0340029","99110CA0340035","99110CA0350012","99110CA0350020","99110CA0350072"),"region"] <- 10
		rdata[rdata$COMPANY == "Health_Net" & !is.na(rdata$PLAN_ID) & 
			rdata$PLAN_ID %in% c("67138CA0520019","67138CA0520020","67138CA0520021","67138CA0620019","67138CA0620020","67138CA0620021","67138CA0620027","99110CA0350013","99110CA0350021","99110CA0350073"),"region"] <- 14
		rdata[rdata$COMPANY == "Health_Net" & !is.na(rdata$PLAN_ID) & 
			rdata$PLAN_ID %in% c("67138CA0520004","67138CA0520009","67138CA0520014","67138CA0620004","67138CA0620009","67138CA0620014","67138CA0620022",
				"99110CA0400006","99110CA0400016","99110CA0400026","99110CA0400036","99110CA0400046","99110CA0400052","99110CA0400058","99110CA0400064","99110CA0350046","99110CA0350050","99110CA0350074"),"region"] <- 15
		rdata[rdata$COMPANY == "Health_Net" & !is.na(rdata$PLAN_ID) & 
			rdata$PLAN_ID %in% c("67138CA0520005","67138CA0520010","67138CA0520015","67138CA0620005","67138CA0620010","67138CA0620015","67138CA0620023",
				"99110CA0400007","99110CA0400017","99110CA0400027","99110CA0400037","99110CA0400047","99110CA0400053","99110CA0400059","99110CA0400065"),"region"] <- 16
		rdata[rdata$COMPANY == "Health_Net" & !is.na(rdata$PLAN_ID) & 
			rdata$PLAN_ID %in% c("67138CA0520006","67138CA0520011","67138CA0520016","67138CA0620006","67138CA0620011","67138CA0620016","67138CA0620024",
				"99110CA0400008","99110CA0400018","99110CA0400028","99110CA0400038","99110CA0400048","99110CA0400054","99110CA0400060","99110CA0400066","99110CA0350047","99110CA0350051","99110CA0350075"),"region"] <- 17
		rdata[rdata$COMPANY == "Health_Net" & !is.na(rdata$PLAN_ID) & 
			rdata$PLAN_ID %in% c("67138CA0520007","67138CA0520012","67138CA0520017","67138CA0620007","67138CA0620012","67138CA0620017","67138CA0620025",
				"99110CA0400009","99110CA0400019","99110CA0400029","99110CA0400039","99110CA0400049","99110CA0400055","99110CA0400061","99110CA0400067","99110CA0350048","99110CA0350052","99110CA0350076"),"region"] <- 18
		rdata[rdata$COMPANY == "Health_Net" & !is.na(rdata$PLAN_ID) & 
			rdata$PLAN_ID %in% c("67138CA0520008","67138CA0520013","67138CA0520018","67138CA0620008","67138CA0620013","67138CA0620018","67138CA0620026",
				"99110CA0400010","99110CA0400020","99110CA0400030","99110CA0400040","99110CA0400050","99110CA0400056","99110CA0400062","99110CA0400068","99110CA0350049","99110CA0350053","99110CA0350077"),"region"] <- 19
		
			# Assign Health Net Silver HMO region 14 year 2014 to region 0
			rdata[rdata$COMPANY == "Health_Net" & rdata$region == 14 & rdata$year == 2014 & rdata$METAL == "Silver","region"] <- 0
		
			# We have to fold the bronze PPO plans in years 2016-2017 in regions 3,15-19 into region 0 b/c there is no experience
			rdata[rdata$insurer_small == "Health_Net" & rdata$region %in% c(15:19) & rdata$year %in% c(2016,2017) & rdata$METAL == "Bronze","region"] <- 0
		
			# We have to fold PPO plans in 2016/7 in region 3 into region 0 b/c there is no experience
			rdata[rdata$insurer_small == "Health_Net" & rdata$region %in% c(3) & rdata$year %in% c(2016,2017) & rdata$METAL == "Bronze","region"] <- 0
			rdata[rdata$insurer_small == "Health_Net" & rdata$region %in% c(3) & rdata$year %in% c(2016,2017) & rdata$METAL == "Silver","region"] <- 0
			rdata[rdata$insurer_small == "Health_Net" & rdata$region %in% c(3) & rdata$year %in% c(2016,2017) & rdata$METAL == "Gold","region"] <- 0
			rdata[rdata$insurer_small == "Health_Net" & rdata$region %in% c(3) & rdata$year %in% c(2016,2017) & rdata$METAL == "Platinum","region"] <- 0
		
			# We have to fold PPO plans in 2014 in regions 7,8,9,10 into region 0 b/c there is no experience
			fix_regions <- c(7,8,9,10)
			rdata[rdata$insurer_small == "Health_Net" & rdata$region %in% fix_regions & rdata$year %in% c(2014) & rdata$METAL == "Bronze","region"] <- 0
			rdata[rdata$insurer_small == "Health_Net" & rdata$region %in% fix_regions & rdata$year %in% c(2014) & rdata$METAL == "Silver","region"] <- 0
			rdata[rdata$insurer_small == "Health_Net" & rdata$region %in% fix_regions & rdata$year %in% c(2014) & rdata$METAL == "Gold","region"] <- 0
			rdata[rdata$insurer_small == "Health_Net" & rdata$region %in% fix_regions & rdata$year %in% c(2014) & rdata$METAL == "Platinum","region"] <- 0
		
			# We have to fold PPO plans in 2015 in regions 2,4,5,7,8,9,10 into region 0 b/c there is no experience
			fix_regions <- c(2,4,5,7,8,9,10)
			rdata[rdata$insurer_small == "Health_Net" & rdata$region %in% fix_regions & rdata$year %in% c(2015) & rdata$METAL == "Bronze","region"] <- 0
			rdata[rdata$insurer_small == "Health_Net" & rdata$region %in% fix_regions & rdata$year %in% c(2015) & rdata$METAL == "Silver","region"] <- 0
			rdata[rdata$insurer_small == "Health_Net" & rdata$region %in% fix_regions & rdata$year %in% c(2015) & rdata$METAL == "Gold","region"] <- 0
			rdata[rdata$insurer_small == "Health_Net" & rdata$region %in% fix_regions & rdata$year %in% c(2015) & rdata$METAL == "Platinum","region"] <- 0
		
			# We have to fold plans in 2015 in region 14 into region 0 b/c there is no experience
			rdata[rdata$insurer_small == "Health_Net" & rdata$region == 14 & rdata$year %in% c(2015) & rdata$METAL == "Silver","region"] <- 0
			rdata[rdata$insurer_small == "Health_Net" & rdata$region == 14 & rdata$year %in% c(2015) & rdata$METAL == "Gold","region"] <- 0
			rdata[rdata$insurer_small == "Health_Net" & rdata$region == 14 & rdata$year %in% c(2015) & rdata$METAL == "Platinum","region"] <- 0
		
			# Zero experience for region 7 silver plans in 2016 and 2017 - fold into region 0
			rdata[rdata$insurer_small == "Health_Net" & rdata$region == 7 & rdata$year %in% c(2016,2017) & rdata$METAL == "Silver","region"] <- 0
			
			# Health Net pulled out of region 7 in 2018, but there are a trivially small number of people enrolled
				# Assign to neighboring region 8
			rdata[rdata$insurer_small == "Health_Net" & rdata$region == 7 & rdata$year == 2018,"region"] <- 8
			
			# No default region 0 in 2014-2017 for HMO plans (combine with PPO)
			rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year %in% c(2014:2017) & rdata$METAL == "Bronze","plan_name"] <- "HN_BR"
			rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year %in% c(2014:2017) & rdata$METAL == "Silver","plan_name"] <- "HN_SIL"
			rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year %in% c(2014:2017) & rdata$METAL == "Gold","plan_name"] <- "HN_G"
			rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year %in% c(2014:2017) & rdata$METAL == "Platinum","plan_name"] <- "HN_P"
			rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year %in% c(2014:2017),"PLAN_TYPE"] <- "PPO"
			
			# Combine 2018 region 0 bronze into PPO
			rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Bronze","plan_name"] <- "HN_BR"
			rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Bronze","PLAN_TYPE"] <- "PPO"
		
			# There are some off-exchange 2018 PPO plans (w/ < 0.1% of experience) that I don't know the region); distribute to other PPO regions
			ppo_silver_2018_plans <- rownames(rdata[rdata$insurer_small == "Health_Net" & rdata$year == 2018 & rdata$region != 0 & rdata$year == 2018 & rdata$METAL == "Silver" & rdata$PLAN_TYPE != "HMO",])
			ppo_gold_2018_plans <- rownames(rdata[rdata$insurer_small == "Health_Net" & rdata$year == 2018 & rdata$region != 0 & rdata$year == 2018 & rdata$METAL == "Gold" & rdata$PLAN_TYPE != "HMO",])
			ppo_platinum_2018_plans <- rownames(rdata[rdata$insurer_small == "Health_Net" & rdata$year == 2018 & rdata$region != 0 & rdata$year == 2018 & rdata$METAL == "Platinum" & rdata$PLAN_TYPE != "HMO",])
		
			rdata[ppo_silver_2018_plans,"EXP_MM"] <- rdata[ppo_silver_2018_plans,"EXP_MM"] + rdata[ppo_silver_2018_plans,"EXP_MM"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Silver" & rdata$PLAN_TYPE != "HMO","EXP_MM"])/
				sum(rdata[ppo_silver_2018_plans,"EXP_MM"])
			rdata[ppo_silver_2018_plans,"PRJ_MM"] <- rdata[ppo_silver_2018_plans,"PRJ_MM"] + rdata[ppo_silver_2018_plans,"PRJ_MM"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Silver" & rdata$PLAN_TYPE != "HMO","PRJ_MM"])/
				sum(rdata[ppo_silver_2018_plans,"PRJ_MM"])
			rdata[ppo_silver_2018_plans,"EXP_INC_CLM"] <- rdata[ppo_silver_2018_plans,"EXP_INC_CLM"] + rdata[ppo_silver_2018_plans,"EXP_INC_CLM"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Silver" & rdata$PLAN_TYPE != "HMO","EXP_INC_CLM"])/
				sum(rdata[ppo_silver_2018_plans,"EXP_INC_CLM"])
			rdata[ppo_silver_2018_plans,"PRJ_INC_CLM"] <- rdata[ppo_silver_2018_plans,"PRJ_INC_CLM"] + rdata[ppo_silver_2018_plans,"PRJ_INC_CLM"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Silver" & rdata$PLAN_TYPE != "HMO","PRJ_INC_CLM"])/
				sum(rdata[ppo_silver_2018_plans,"PRJ_INC_CLM"])
			rdata[ppo_silver_2018_plans,"EXP_RSK_ADJ"] <- rdata[ppo_silver_2018_plans,"EXP_RSK_ADJ"] + rdata[ppo_silver_2018_plans,"EXP_RSK_ADJ"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Silver" & rdata$PLAN_TYPE != "HMO","EXP_RSK_ADJ"])/
				sum(rdata[ppo_silver_2018_plans,"EXP_RSK_ADJ"])
			rdata[ppo_silver_2018_plans,"PRJ_RSK_ADJ"] <- rdata[ppo_silver_2018_plans,"PRJ_RSK_ADJ"] + rdata[ppo_silver_2018_plans,"PRJ_RSK_ADJ"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Silver" & rdata$PLAN_TYPE != "HMO","PRJ_RSK_ADJ"])/
				sum(rdata[ppo_silver_2018_plans,"PRJ_RSK_ADJ"])
			rdata[ppo_silver_2018_plans,"EXP_REIN"] <- rdata[ppo_silver_2018_plans,"EXP_REIN"] + rdata[ppo_silver_2018_plans,"EXP_REIN"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Silver" & rdata$PLAN_TYPE != "HMO","EXP_REIN"])/
				sum(rdata[ppo_silver_2018_plans,"EXP_REIN"])
			rdata[ppo_silver_2018_plans,"PRJ_REIN"] <- rdata[ppo_silver_2018_plans,"PRJ_REIN"] + rdata[ppo_silver_2018_plans,"PRJ_REIN"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Silver" & rdata$PLAN_TYPE != "HMO","PRJ_REIN"])/
				sum(rdata[ppo_silver_2018_plans,"PRJ_REIN"])
			rdata[ppo_silver_2018_plans,"EXP_TP"] <- rdata[ppo_silver_2018_plans,"EXP_TP"] + rdata[ppo_silver_2018_plans,"EXP_TP"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Silver" & rdata$PLAN_TYPE != "HMO","EXP_TP"])/
				sum(rdata[ppo_silver_2018_plans,"EXP_TP"])
			rdata[ppo_silver_2018_plans,"PRJ_TP"] <- rdata[ppo_silver_2018_plans,"PRJ_TP"] + rdata[ppo_silver_2018_plans,"PRJ_TP"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Silver" & rdata$PLAN_TYPE != "HMO","PRJ_TP"])/
				sum(rdata[ppo_silver_2018_plans,"PRJ_TP"])
			
			rdata[ppo_gold_2018_plans,"EXP_MM"] <- rdata[ppo_gold_2018_plans,"EXP_MM"] + rdata[ppo_gold_2018_plans,"EXP_MM"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Gold" & rdata$PLAN_TYPE != "HMO","EXP_MM"])/
				sum(rdata[ppo_gold_2018_plans,"EXP_MM"])
			rdata[ppo_gold_2018_plans,"PRJ_MM"] <- rdata[ppo_gold_2018_plans,"PRJ_MM"] + rdata[ppo_gold_2018_plans,"PRJ_MM"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Gold" & rdata$PLAN_TYPE != "HMO","PRJ_MM"])/
				sum(rdata[ppo_gold_2018_plans,"PRJ_MM"])
			rdata[ppo_gold_2018_plans,"EXP_INC_CLM"] <- rdata[ppo_gold_2018_plans,"EXP_INC_CLM"] + rdata[ppo_gold_2018_plans,"EXP_INC_CLM"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Gold" & rdata$PLAN_TYPE != "HMO","EXP_INC_CLM"])/
				sum(rdata[ppo_gold_2018_plans,"EXP_INC_CLM"])
			rdata[ppo_gold_2018_plans,"PRJ_INC_CLM"] <- rdata[ppo_gold_2018_plans,"PRJ_INC_CLM"] + rdata[ppo_gold_2018_plans,"PRJ_INC_CLM"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Gold" & rdata$PLAN_TYPE != "HMO","PRJ_INC_CLM"])/
				sum(rdata[ppo_gold_2018_plans,"PRJ_INC_CLM"])
			rdata[ppo_gold_2018_plans,"EXP_RSK_ADJ"] <- rdata[ppo_gold_2018_plans,"EXP_RSK_ADJ"] + rdata[ppo_gold_2018_plans,"EXP_RSK_ADJ"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Gold" & rdata$PLAN_TYPE != "HMO","EXP_RSK_ADJ"])/
				sum(rdata[ppo_gold_2018_plans,"EXP_RSK_ADJ"])
			rdata[ppo_gold_2018_plans,"PRJ_RSK_ADJ"] <- rdata[ppo_gold_2018_plans,"PRJ_RSK_ADJ"] + rdata[ppo_gold_2018_plans,"PRJ_RSK_ADJ"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Gold" & rdata$PLAN_TYPE != "HMO","PRJ_RSK_ADJ"])/
				sum(rdata[ppo_gold_2018_plans,"PRJ_RSK_ADJ"])
			rdata[ppo_gold_2018_plans,"EXP_REIN"] <- rdata[ppo_gold_2018_plans,"EXP_REIN"] + rdata[ppo_gold_2018_plans,"EXP_REIN"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Gold" & rdata$PLAN_TYPE != "HMO","EXP_REIN"])/
				sum(rdata[ppo_gold_2018_plans,"EXP_REIN"])
			rdata[ppo_gold_2018_plans,"PRJ_REIN"] <- rdata[ppo_gold_2018_plans,"PRJ_REIN"] + rdata[ppo_gold_2018_plans,"PRJ_REIN"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Gold" & rdata$PLAN_TYPE != "HMO","PRJ_REIN"])/
				sum(rdata[ppo_gold_2018_plans,"PRJ_REIN"])
			rdata[ppo_gold_2018_plans,"EXP_TP"] <- rdata[ppo_gold_2018_plans,"EXP_TP"] + rdata[ppo_gold_2018_plans,"EXP_TP"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Gold" & rdata$PLAN_TYPE != "HMO","EXP_TP"])/
				sum(rdata[ppo_gold_2018_plans,"EXP_TP"])
			rdata[ppo_gold_2018_plans,"PRJ_TP"] <- rdata[ppo_gold_2018_plans,"PRJ_TP"] + rdata[ppo_gold_2018_plans,"PRJ_TP"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Gold" & rdata$PLAN_TYPE != "HMO","PRJ_TP"])/
				sum(rdata[ppo_gold_2018_plans,"PRJ_TP"])
		
			rdata[ppo_platinum_2018_plans,"EXP_MM"] <- rdata[ppo_platinum_2018_plans,"EXP_MM"] + rdata[ppo_platinum_2018_plans,"EXP_MM"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Platinum" & rdata$PLAN_TYPE != "HMO","EXP_MM"])/
				sum(rdata[ppo_platinum_2018_plans,"EXP_MM"])
			rdata[ppo_platinum_2018_plans,"PRJ_MM"] <- rdata[ppo_platinum_2018_plans,"PRJ_MM"] + rdata[ppo_platinum_2018_plans,"PRJ_MM"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Platinum" & rdata$PLAN_TYPE != "HMO","PRJ_MM"])/
				sum(rdata[ppo_platinum_2018_plans,"PRJ_MM"])
			rdata[ppo_platinum_2018_plans,"EXP_INC_CLM"] <- rdata[ppo_platinum_2018_plans,"EXP_INC_CLM"] + rdata[ppo_platinum_2018_plans,"EXP_INC_CLM"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Platinum" & rdata$PLAN_TYPE != "HMO","EXP_INC_CLM"])/
				sum(rdata[ppo_platinum_2018_plans,"EXP_INC_CLM"])
			rdata[ppo_platinum_2018_plans,"PRJ_INC_CLM"] <- rdata[ppo_platinum_2018_plans,"PRJ_INC_CLM"] + rdata[ppo_platinum_2018_plans,"PRJ_INC_CLM"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Platinum" & rdata$PLAN_TYPE != "HMO","PRJ_INC_CLM"])/
				sum(rdata[ppo_platinum_2018_plans,"PRJ_INC_CLM"])
			rdata[ppo_platinum_2018_plans,"EXP_RSK_ADJ"] <- rdata[ppo_platinum_2018_plans,"EXP_RSK_ADJ"] + rdata[ppo_platinum_2018_plans,"EXP_RSK_ADJ"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Platinum" & rdata$PLAN_TYPE != "HMO","EXP_RSK_ADJ"])/
				sum(rdata[ppo_platinum_2018_plans,"EXP_RSK_ADJ"])
			rdata[ppo_platinum_2018_plans,"PRJ_RSK_ADJ"] <- rdata[ppo_platinum_2018_plans,"PRJ_RSK_ADJ"] + rdata[ppo_platinum_2018_plans,"PRJ_RSK_ADJ"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Platinum" & rdata$PLAN_TYPE != "HMO","PRJ_RSK_ADJ"])/
				sum(rdata[ppo_platinum_2018_plans,"PRJ_RSK_ADJ"])
			rdata[ppo_platinum_2018_plans,"EXP_REIN"] <- rdata[ppo_platinum_2018_plans,"EXP_REIN"] + rdata[ppo_platinum_2018_plans,"EXP_REIN"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Platinum" & rdata$PLAN_TYPE != "HMO","EXP_REIN"])/
				sum(rdata[ppo_platinum_2018_plans,"EXP_REIN"])
			rdata[ppo_platinum_2018_plans,"PRJ_REIN"] <- rdata[ppo_platinum_2018_plans,"PRJ_REIN"] + rdata[ppo_platinum_2018_plans,"PRJ_REIN"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Platinum" & rdata$PLAN_TYPE != "HMO","PRJ_REIN"])/
				sum(rdata[ppo_platinum_2018_plans,"PRJ_REIN"])
			rdata[ppo_platinum_2018_plans,"EXP_TP"] <- rdata[ppo_platinum_2018_plans,"EXP_TP"] + rdata[ppo_platinum_2018_plans,"EXP_TP"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Platinum" & rdata$PLAN_TYPE != "HMO","EXP_TP"])/
				sum(rdata[ppo_platinum_2018_plans,"EXP_TP"])
			rdata[ppo_platinum_2018_plans,"PRJ_TP"] <- rdata[ppo_platinum_2018_plans,"PRJ_TP"] + rdata[ppo_platinum_2018_plans,"PRJ_TP"] *
				sum(rdata[rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL == "Platinum" & rdata$PLAN_TYPE != "HMO","PRJ_TP"])/
				sum(rdata[ppo_platinum_2018_plans,"PRJ_TP"])
		
			# And now delete region 0 plans
			rdata <- rdata[!(rdata$insurer_small == "Health_Net" & rdata$region == 0 & rdata$year == 2018 & rdata$METAL != "Bronze" & rdata$PLAN_TYPE != "HMO"),]
		
		# Chinese Community is in regions 4 and 8
			# NOTE: there are off-exchange plans I couldn't classify, but they are extremely small
		rdata[rdata$COMPANY == "Chinese_Community","region"] <- 4
		rdata[rdata$COMPANY == "Chinese_Community" & !is.na(rdata$PROD_ID) &  
			rdata$PROD_ID %in% c("47579CA027","47579CA029","47579CA031","47579CA033","47579CA035","47579CA054","47579CA061"),"region"] <- 8
		
		# Valley is only in region 7
		rdata[rdata$COMPANY == "Valley","region"] <- 7

		# SHARP is only in region 19, but I can't tell it apart from Molina
		#rdata[rdata$COMPANY == "Sharp","region"] <- 19
		
		# Western is in regions 2 and 3
		rdata[rdata$COMPANY == "Western","region"] <- 3
		rdata[rdata$COMPANY == "Western" & !is.na(rdata$PROD_ID) &  
			rdata$PROD_ID %in% c("93689CA011","93689CA012","93689CA013"),"region"] <- 2
		
			# For 2014 only, we can't distinguish the rating area
			rdata[rdata$COMPANY == "Western" & rdata$year == 2014,"region"] <- 0
		
		# Contra Costa is only in region 5
		rdata[rdata$COMPANY == "Contra_Costa","region"] <- 5
		
	# Now map every plan in plans_pmt to a plan in rdata
	rdata$pmt_name <- paste(rdata$plan_name,rdata$year,rdata$region,sep="")	
	plans_pmt$rate_name <- plans_pmt$pmt_name	
	plans_pmt[!plans_pmt$rate_name %in% rdata$pmt_name,"rate_name"]	<- 
		paste(plans_pmt[!plans_pmt$rate_name %in% rdata$pmt_name,"plan_name"],plans_pmt[!plans_pmt$rate_name %in% rdata$pmt_name,"year"],0,sep="")
	
		# Fix 2 remaining discrepancies
		plans_pmt[plans_pmt$rate_name == "BS_G320170","rate_name"] <- "BS_G320174" # B/c we combined regions 2 and 4
		rdata[rdata$COMPANY == "Health_Net" & rdata$year == 2015 & rdata$METAL == "Bronze" & rdata$ISSUER_ID == 67138,"pmt_name"] <- "HN_BR320150"
		rdata[rdata$COMPANY == "Health_Net" & rdata$year == 2015 & rdata$METAL == "Bronze" & rdata$ISSUER_ID == 67138,"plan_name"] <- "HN_BR3"
		rdata[rdata$COMPANY == "Health_Net" & rdata$year == 2015 & rdata$METAL == "Bronze" & rdata$ISSUER_ID == 67138,"PLAN_TYPE"] <- "HMO"
		
#### Now we build the rate filing data object to use for regressions/estimation

	rsdata <- rdata[!duplicated(rdata$pmt_name),c("pmt_name","plan_name","region","year","insurer_small","METAL","AV_METAL","PLAN_TYPE")]
	rownames(rsdata) <- rsdata$pmt_name
	
	rsdata$EXP_MM <- by(rdata$EXP_MM,rdata$pmt_name,sum)[rownames(rsdata)]
	rsdata$EXP_TP <- by(rdata$EXP_TP,rdata$pmt_name,sum)[rownames(rsdata)]
	rsdata$EXP_INC_CLM <- by(rdata$EXP_INC_CLM,rdata$pmt_name,sum)[rownames(rsdata)]
	rsdata$EXP_RSK_ADJ <- by(rdata$EXP_RSK_ADJ,rdata$pmt_name,sum)[rownames(rsdata)]
	rsdata$EXP_REIN <- by(rdata$EXP_REIN,rdata$pmt_name,sum)[rownames(rsdata)]
	rsdata$EXP_TP_PMPM <- rsdata$EXP_TP/rsdata$EXP_MM
	rsdata$EXP_INC_CLM_PMPM <- rsdata$EXP_INC_CLM/rsdata$EXP_MM
	rsdata$EXP_RSK_ADJ_PMPM <- rsdata$EXP_RSK_ADJ/rsdata$EXP_MM
	rsdata$EXP_REIN_PMPM <- rsdata$EXP_REIN/rsdata$EXP_MM
	
	rsdata$PRJ_MM <- by(rdata$PRJ_MM,rdata$pmt_name,sum,na.rm=T)[rownames(rsdata)]
	rsdata$PRJ_TP <- by(rdata$PRJ_TP,rdata$pmt_name,sum,na.rm=T)[rownames(rsdata)]
	rsdata$PRJ_INC_CLM <- by(rdata$PRJ_INC_CLM,rdata$pmt_name,sum,na.rm=T)[rownames(rsdata)]
	rsdata$PRJ_RSK_ADJ <- by(rdata$PRJ_RSK_ADJ,rdata$pmt_name,sum,na.rm=T)[rownames(rsdata)]
	rsdata$PRJ_REIN <- by(rdata$PRJ_REIN,rdata$pmt_name,sum,na.rm=T)[rownames(rsdata)]
	rsdata$PRJ_INC_CLM_PMPM <- rsdata$PRJ_INC_CLM/rsdata$PRJ_MM
	rsdata$PRJ_RSK_ADJ_PMPM <- rsdata$PRJ_RSK_ADJ/rsdata$PRJ_MM
	rsdata$PRJ_REIN_PMPM <- rsdata$PRJ_REIN/rsdata$PRJ_MM
	
	# Verify that Plan type variable is correct
	rsdata[rsdata$plan_name %in% c("ANT_BR","ANT_SIL","ANT_G","ANT_P","BS_BR","BS_SIL","BS_G","BS_P",
		"HN_BR","HN_SIL","HN_G","HN_P"),"PLAN_TYPE"] <- "PPO"
	rsdata[rsdata$plan_name %in% c("ANT_SIL3","ANT_G3","ANT_P3","BS_SIL3","BS_G3","BS_P3",
		"HN_BR3","HN_SIL3","HN_G3","HN_P3"),"PLAN_TYPE"] <- "HMO"
	
#### Calculate risk scores

	# CMS definitions for AV and moral hazard by metal
	avs <- c(0.60,0.70,0.80,0.90)
	moral_hazard <- c(1,1.03,1.08,1.15)
	total_adjustment <- avs * moral_hazard
	names(total_adjustment) <- c("Bronze","Silver","Gold","Platinum")

	rsdata$Silver <- as.numeric(rsdata$METAL == "Silver")
	rsdata$Gold <- as.numeric(rsdata$METAL == "Gold")
	rsdata$Platinum <- as.numeric(rsdata$METAL == "Platinum")

	# Need:
		# 1) plan risk adjustment transfer
		# 2) plan risk-adjusted share
		# 3) plan demand
		# 4) total premium collected across all plans
		# 5) total demand in market
		
		rsdata$risk_score <- NA
		for(year in c(2014:2018)) {
			total_premium <- sum(rsdata[rsdata$year == year,"EXP_TP"])
			rsdata[rsdata$year == year,"s_av"] <- (total_adjustment[rsdata[rsdata$year == year,"METAL"]] * rsdata[rsdata$year == year,"EXP_MM"])/
				sum(total_adjustment[rsdata[rsdata$year == year,"METAL"]] * rsdata[rsdata$year == year,"EXP_MM"]) 
			rsdata[rsdata$year == year,"share"] <- rsdata[rsdata$year == year,"EXP_MM"]/sum(rsdata[rsdata$year == year,"EXP_MM"]) 
			#rsdata[rsdata$year == year,"risk_score"] <- 1 + rsdata[rsdata$year == year,"EXP_RSK_ADJ"]/(rsdata[rsdata$year == year,"s_av"] * total_premium)
			rsdata[rsdata$year == year,"risk_score"] <- (rsdata[rsdata$year == year,"EXP_RSK_ADJ"]/total_premium + rsdata[rsdata$year == year,"s_av"])/rsdata[rsdata$year == year,"share"]
		}
		rsdata$log_risk_score <- log(rsdata$risk_score)
	
		# Tabulate risk adjustment by metal and year
		ra_metals <- c("Bronze","Silver","Gold","Platinum")
		years <- c(2014:2018)
		ra_output <- matrix(NA,length(ra_metals),length(years),dimnames=list(ra_metals,years))
		for(t in 1:length(years)) {
			for(m in 1:length(ra_metals)) {
				ra_output[m,t] <- sum(rsdata[rsdata$year == years[t] & rsdata$METAL == ra_metals[m],"EXP_RSK_ADJ"],na.rm=T)/
										sum(rsdata[rsdata$year == years[t] & rsdata$METAL == ra_metals[m],"EXP_MM"],na.rm=T)
			}
		}
		
		write.csv(ra_output,"data/final/ra_by_metal_year.csv")
		
		
	

##### Now we want to prepare the rate filing data for estimation of risk scores
		
	# Add variables

	rsdata[is.na(rsdata$AV_METAL) & rsdata$METAL == "Silver","AV_METAL"] <- 0.7 
	rsdata$AV_Demean <- rsdata$AV_METAL - 0.7
	rsdata$HMO <- as.numeric(rsdata$PLAN_TYPE == "HMO")
	rsdata[is.na(rsdata$HMO),"HMO"] <- 1 # solves issue with Contra Costa
	rsdata$trend <- rsdata$year - min(rsdata$year)
	rsdata$Anthem <- as.numeric(rsdata$insurer_small == "Anthem")
	rsdata$Blue_Shield <- as.numeric(rsdata$insurer_small == "Blue_Shield")
	rsdata$Health_Net <- as.numeric(rsdata$insurer_small == "Health_Net")
	rsdata$Kaiser <- as.numeric(rsdata$insurer_small == "Kaiser")
	
	rsdata$log_cost <- NA
	rsdata[rsdata$EXP_INC_CLM_PMPM > 0 & rsdata$EXP_MM > 0,"log_cost"] <- 
		log(rsdata[rsdata$EXP_INC_CLM_PMPM > 0 & rsdata$EXP_MM > 0,"EXP_INC_CLM_PMPM"])
	
##### Prepare Demand Data
	
	# Prepare demand data

		#setwd("C:/Users/esaltzm/OneDrive - Emory University/Covered California") # Office computer directory
		demand_data <- read.csv(file="data/final/julia_data_small.csv",header=TRUE)
		households <- get(load("data/final/households_small"))
		
		rsdata <- rsdata[rsdata$year <= year_to_run,]
		demand_data <- demand_data[demand_data$year <= year_to_run,]			
		filename <- paste0("data/final/choice_probs_2014_",year_to_run,".csv")
		probs <- read.csv(filename,header=TRUE)
		demand_data$prob <- probs
		rm(probs)
		gc()
		
		#demand_data <- demand_data_all[demand_data$year < 2019,]	
		#probs2018 <- read.csv(file="choice_probs_2014_2018.csv",header=TRUE)
		#probs <- read.csv(file="choice_probs_2014_2018_full.csv",header=TRUE)
		#probs <- read.csv(file="choice_probs_2014_2018_nodem_noimfe.csv",header=TRUE)
		#probs <- read.csv(file="choice_probs_2014_2019.csv",header=TRUE)
		
		# Drop uninsured
		demand_data <- demand_data[demand_data$uninsured_plan == 0,]
		
		# We need to get rid of CSR plans in demand_data
		demand_data$plan_name <- as.character(demand_data$plan_name)
		demand_data[demand_data$plan_name %in% c("ANT_SIL73","ANT_SIL87","ANT_SIL94"),"plan_name"] <- "ANT_SIL"
		demand_data[demand_data$plan_name %in% c("ANT_SIL733","ANT_SIL873","ANT_SIL943"),"plan_name"] <- "ANT_SIL3"
		demand_data[demand_data$plan_name %in% c("BS_SIL73","BS_SIL87","BS_SIL94"),"plan_name"] <- "BS_SIL"
		demand_data[demand_data$plan_name %in% c("BS_SIL733","BS_SIL873","BS_SIL943"),"plan_name"] <- "BS_SIL3"
		demand_data[demand_data$plan_name %in% c("HN_SIL73","HN_SIL87","HN_SIL94"),"plan_name"] <- "HN_SIL"
		demand_data[demand_data$plan_name %in% c("HN_SIL733","HN_SIL873","HN_SIL943"),"plan_name"] <- "HN_SIL3"
		demand_data[demand_data$plan_name %in% c("KA_SIL73","KA_SIL87","KA_SIL94"),"plan_name"] <- "KA_SIL"
		demand_data[demand_data$plan_name %in% c("SMALL_SIL73","SMALL_SIL87","SMALL_SIL94"),"plan_name"] <- "SMALL_SIL"
				
		# We need to get rid of coinsurance, HSA and catastrophic plans in demand data
		demand_data[demand_data$plan_name %in% c("ANT_BR_HSA","ANT_CAT"),"plan_name"] <- "ANT_BR"
		demand_data[demand_data$plan_name %in% c("BS_BR_HSA","BS_CAT"),"plan_name"] <- "BS_BR"
		demand_data[demand_data$plan_name %in% c("HN_CAT"),"plan_name"] <- "HN_BR"
		demand_data[demand_data$plan_name %in% c("HN_CAT3"),"plan_name"] <- "HN_BR3"
		demand_data[demand_data$plan_name %in% c("KA_BR_HSA","KA_CAT"),"plan_name"] <- "KA_BR"
		demand_data[demand_data$plan_name %in% c("KA_G_COIN"),"plan_name"] <- "KA_G"
		demand_data[demand_data$plan_name %in% c("SMALL_BR_HSA","SMALL_CAT"),"plan_name"] <- "SMALL_BR"
		
		# Data unique id

			demand_data$region <- households[demand_data$household_number,"rating_area"]
			demand_data$pmt_name <- paste(demand_data$plan_name,demand_data$year,demand_data$region,sep="")
			demand_data$rate_name <- plans_pmt[demand_data$pmt_name,"rate_name"]
			
			
	# Create Demographic Shares
	
		rating_area_fields <- c("share_ra1","share_ra2","share_ra3","share_ra4","share_ra5","share_ra6","share_ra7",
								"share_ra8","share_ra9","share_ra10","share_ra11","share_ra12","share_ra13",
								"share_ra14","share_ra15","share_ra16","share_ra17","share_ra18","share_ra19")
		rsdata[,c("demand","share_0to17","share_18to34","share_18to25","share_26to34","share_26to44","share_35to54",
			"share_35to44","share_45to54","share_55+","share_0to250","share_250to400","share_gt400","share_male",
			"share_SEP",rating_area_fields,"share_Asian","share_Black","share_Hispanic","share_Other_Race","share_White")] <- NA
		
		mapped_plans <- rownames(rsdata)[rsdata$pmt_name %in% plans_pmt$rate_name] 
		
		rsdata[mapped_plans,"demand"] <- by(demand_data$prob,demand_data$rate_name,sum)[mapped_plans]
		rsdata[mapped_plans,"share_0to17"] <- by(households[demand_data$household_number,"perc_0to17"] * 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]
		rsdata[mapped_plans,"share_0to25"] <- by((households[demand_data$household_number,"perc_0to17"] + 
			households[demand_data$household_number,"perc_18to25"])	* 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]
		rsdata[mapped_plans,"share_18to34"] <- by((households[demand_data$household_number,"perc_26to34"] + 
			households[demand_data$household_number,"perc_18to25"])	* 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]
		rsdata[mapped_plans,"share_18to25"] <- by(households[demand_data$household_number,"perc_18to25"] * 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]
		rsdata[mapped_plans,"share_26to34"] <- by(households[demand_data$household_number,"perc_26to34"] * 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]
		rsdata[mapped_plans,"share_35to44"] <- by(households[demand_data$household_number,"perc_35to44"] * 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]
		rsdata[mapped_plans,"share_45to54"] <- by(households[demand_data$household_number,"perc_45to54"] * 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]
		rsdata[mapped_plans,"share_35to54"] <- by((households[demand_data$household_number,"perc_35to44"] + 
			households[demand_data$household_number,"perc_45to54"])	* 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]
		rsdata[mapped_plans,"share_26to54"] <- by((households[demand_data$household_number,"perc_35to44"] + 
			households[demand_data$household_number,"perc_45to54"] + households[demand_data$household_number,"perc_26to34"])	* 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]
		rsdata[mapped_plans,"share_26to44"] <- by((households[demand_data$household_number,"perc_35to44"] + 
			households[demand_data$household_number,"perc_26to34"])	* 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]
		rsdata[mapped_plans,"share_0to34"] <- rsdata[mapped_plans,"share_0to17"] + rsdata[mapped_plans,"share_18to34"]
		rsdata[mapped_plans,"share_0to250"] <- by(as.numeric(households[demand_data$household_number,"FPL"] <= 2.5) * 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]
		rsdata[mapped_plans,"share_250to400"] <- by(as.numeric(households[demand_data$household_number,"FPL"] > 2.5 &
			households[demand_data$household_number,"FPL"] <= 4) * demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]
		rsdata[mapped_plans,"share_gt400"] <- by(as.numeric(households[demand_data$household_number,"FPL"] > 4) * 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]
		rsdata[mapped_plans,"share_SEP"] <- by(households[demand_data$household_number,"SEP"] * 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]	
		rsdata[mapped_plans,"share_male"] <- by(households[demand_data$household_number,"perc_male"] * 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]	
		rsdata[mapped_plans,"share_Asian"] <- by(households[demand_data$household_number,"perc_asian"] * 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]	
		rsdata[mapped_plans,"share_Black"] <- by(households[demand_data$household_number,"perc_black"] * 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]	
		rsdata[mapped_plans,"share_Hispanic"] <- by(households[demand_data$household_number,"perc_hispanic"] * 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]	
		rsdata[mapped_plans,"share_Other_Race"] <- by(households[demand_data$household_number,"perc_other"] * 
			demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]	
		rsdata$share_minority <- rsdata$share_Asian + rsdata$share_Black + 
			rsdata$share_Hispanic + rsdata$share_Other_Race
			
		for(m in 1:length(rating_area_fields)) {
			rsdata[mapped_plans,rating_area_fields[m]] <- 
				by(as.numeric(households[demand_data$household_number,"rating_area"] == m) * 
					demand_data$prob,demand_data$rate_name,sum)[mapped_plans]/rsdata[mapped_plans,"demand"]
		}
		
##### Run risk score regressions	
		
	
	# Specification for job market paper (V9)
	spec_mktfe <- log_risk_score ~ AV_Demean + 
				share_18to25 + share_26to34 + share_35to54 + 
				share_male
	risk_score_jmp <- lm(spec_mktfe,data=rsdata,weights=rsdata$EXP_MM)
	summary(risk_score_jmp)
	
	# Specification for Inertia paper (V11)
	
	spec_mktfe <- log_risk_score ~ Silver + Gold + Platinum + share_18to34 + share_35to54 + share_male
	risk_score_alt <- lm(spec_mktfe,data=rsdata,weights=rsdata$EXP_MM)
	summary(risk_score_alt)
	
	
	# Specification for learning paper and this paper (V11_learn)
	spec_mktfe <- log_risk_score ~ Silver + Gold + Platinum + share_18to34 + share_35to54 + share_Hispanic
	risk_score_alt_learn <- lm(spec_mktfe,data=rsdata,weights=rsdata$EXP_MM)
	summary(risk_score_alt_learn)
	
	filename <- paste0("data/final/rs_coefficients_nodem_",year_to_run)
	write.csv(coef(risk_score_alt_learn),filename)
	write.csv(summary(risk_score_alt_learn)[5],paste("data/final/reg_output_",year_to_run,".csv",sep=""))
	if(year_to_run == 2018) save(risk_score_alt,file="data/final/rs_reg")
	
	
	use_alt_version <- TRUE
	learning <- TRUE
	if(use_alt_version & !learning) {
		risk_score_mktfe <- risk_score_alt
	} else if (use_alt_version) {
		risk_score_mktfe <- risk_score_alt_learn
	} else {
		filename <- paste0("data/final/rs_coefficients_nodem_",year_to_run,".csv")
		write.csv(coef(risk_score_mktfe),filename)
		write.csv(summary(risk_score_mktfe)[5],"data/final/reg_output.csv")
		if(year_to_run == 2018) save(risk_score_mktfe,file="data/final/rs_reg")
	}
	
	
	
# Run Average Claims Regression on rsdata observations only

	

	rsdata$predicted_risk_score <- rsdata$log_risk_score <- NA
	rsdata[mapped_plans,"predicted_risk_score"] <- exp(predict(risk_score_mktfe,newdata=rsdata[mapped_plans,]))
	rsdata[mapped_plans,"log_risk_score"] <- predict(risk_score_mktfe,newdata=rsdata[mapped_plans,])

	if(year_to_run == 2014) {
		spec <- log_cost ~ log_risk_score + HMO + Anthem + Blue_Shield + Health_Net + Kaiser +
					share_ra2 + share_ra3 + share_ra4 + share_ra5 + share_ra6 + share_ra7 +  
					share_ra8 + share_ra9 + share_ra10 + share_ra11 + share_ra12 + share_ra13 + 
					share_ra14 + share_ra15 + share_ra16 + share_ra17 + share_ra18 + share_ra19
	} else {
		spec <- log_cost ~ log_risk_score + HMO + trend + Anthem + Blue_Shield + Health_Net + Kaiser +
					share_ra2 + share_ra3 + share_ra4 + share_ra5 + share_ra6 + share_ra7 +  
					share_ra8 + share_ra9 + share_ra10 + share_ra11 + share_ra12 + share_ra13 + 
					share_ra14 + share_ra15 + share_ra16 + share_ra17 + share_ra18 + share_ra19
	}
	avg_claims_firmfe <- lm(spec,data=rsdata[mapped_plans,],weights=rsdata[mapped_plans,"EXP_MM"])
	summary(avg_claims_firmfe)
	rsdata$model_predicted_claims <- exp(predict(avg_claims_firmfe))
	filename = paste("data/final/claims_initial_",year_to_run,".csv",sep="")
	write.csv(coef(avg_claims_firmfe),filename)
	
##### Calculate Predicted and Actual Net Cost by Plan	
	
	# Predicted net cost
		# There are 22 plans with negative predicted net cost
		# They account for 0.07% of predicted member months
		# It's theoretically possible to have negative predicted net cost b/c of risk adjustment, but likely an accounting error by the firms
		# There are also 5 plans with zero projected member months/claims/etc.
			# assign these plans 0 predicted net cost (won't matter as long as moments are weighted by proj. member months)
	# There are 34 plans with negative actual net costs, but these won't be used in any regression (only the aggregate matters)
	
	rsdata$predicted_net_cost <- rsdata$PRJ_INC_CLM/rsdata$PRJ_MM - 
		rsdata$PRJ_RSK_ADJ/rsdata$PRJ_MM - rsdata$PRJ_REIN/rsdata$PRJ_MM
	rsdata$actual_net_cost <- rsdata$EXP_INC_CLM/rsdata$EXP_MM - 
		rsdata$EXP_RSK_ADJ/rsdata$EXP_MM - rsdata$EXP_REIN/rsdata$EXP_MM
	
	rsdata[is.na(rsdata$predicted_net_cost),"predicted_net_cost"] <- 0
	
	# Reinsurance factor
		# NOTE: 5 of these records have unreasonably large predicted reinsurance factors
		# They are all from 2014, which we are not trying to predict, so no problem 
	
	rsdata$predicted_reins_factor <- rsdata$PRJ_REIN/rsdata$PRJ_INC_CLM
	rsdata[is.na(rsdata$predicted_reins_factor),"predicted_reins_factor"] <- 0
	
##### Consolidate Predictions

	# Some of the insurers reported the same projections for certain plans
	# We need to consolidate these plans
	
	rsdata$proj_name <- rsdata$pmt_name	
	
	rsdata$AV <- 0.6
	rsdata[rsdata$METAL == "Silver","AV"] <- 0.7
	rsdata[rsdata$METAL == "Gold","AV"] <- 0.8
	rsdata[rsdata$METAL == "Platinum","AV"] <- 0.9
	
	# 2018
		# Anthem: No consolidation needed
		# Blue Shield: consolidate HMO plans across markets by metal
		# Health Net: consolidate by plan type across markets and metals
		# Kaiser: No consolidation needed
		
		# Blue Shield
		rsdata[rsdata$Blue_Shield == 1 & rsdata$HMO == 1 & rsdata$year == 2018 & rsdata$METAL == "Silver","proj_name"] <- "BS_SIL320180"
		rsdata[rsdata$Blue_Shield == 1 & rsdata$HMO == 1 & rsdata$year == 2018 & rsdata$METAL == "Gold","proj_name"] <- "BS_G320180"
		rsdata[rsdata$Blue_Shield == 1 & rsdata$HMO == 1 & rsdata$year == 2018 & rsdata$METAL == "Platinum","proj_name"] <- "BS_P320180"
		
		# Health Net
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2018 & rsdata$METAL == "Silver","proj_name"] <- "HN_SIL320180"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2018 & rsdata$METAL == "Gold","proj_name"] <- "HN_G320180"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2018 & rsdata$METAL == "Platinum","proj_name"] <- "HN_P320180"
		
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2018 & rsdata$METAL == "Bronze","proj_name"] <- "HN_BR20180"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2018 & rsdata$METAL == "Silver","proj_name"] <- "HN_SIL20180"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2018 & rsdata$METAL == "Gold","proj_name"] <- "HN_G20180"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2018 & rsdata$METAL == "Platinum","proj_name"] <- "HN_P20180"
		
		#rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2018,"proj_name"] <- "HN_320180"
		#rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2018,"proj_name"] <- "HN_20180"
		
	# 2017
		# Anthem: No consolidation needed
		# Blue Shield: consolidate HMO plans across markets by metal
		# Health Net: consolidate by plan type across markets and metals
		# Kaiser: No consolidation needed
		
		# Blue Shield
		rsdata[rsdata$Blue_Shield == 1 & rsdata$HMO == 1 & rsdata$year == 2017 & rsdata$METAL == "Silver","proj_name"] <- "BS_SIL320170"
		rsdata[rsdata$Blue_Shield == 1 & rsdata$HMO == 1 & rsdata$year == 2017 & rsdata$METAL == "Gold","proj_name"] <- "BS_G320170"
		rsdata[rsdata$Blue_Shield == 1 & rsdata$HMO == 1 & rsdata$year == 2017 & rsdata$METAL == "Platinum","proj_name"] <- "BS_P320170"
		
		# Health Net
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2017 & rsdata$METAL == "Silver","proj_name"] <- "HN_SIL320170"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2017 & rsdata$METAL == "Gold","proj_name"] <- "HN_G320170"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2017 & rsdata$METAL == "Platinum","proj_name"] <- "HN_P320170"
		
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2017 & rsdata$METAL == "Bronze","proj_name"] <- "HN_BR20170"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2017 & rsdata$METAL == "Silver","proj_name"] <- "HN_SIL20170"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2017 & rsdata$METAL == "Gold","proj_name"] <- "HN_G20170"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2017 & rsdata$METAL == "Platinum","proj_name"] <- "HN_P20170"
		
		#rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2017,"proj_name"] <- "HN_320170"
		#rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2017,"proj_name"] <- "HN_20170"
		
	# 2016:
		# Anthem: No consolidation needed
		# Blue Shield: No consolidation needed
		# Health Net: consolidate by plan type across markets and metals
		# Kaiser: No consolidation needed
		
		# Health Net
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2016 & rsdata$METAL == "Silver","proj_name"] <- "HN_SIL320160"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2016 & rsdata$METAL == "Gold","proj_name"] <- "HN_G320160"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2016 & rsdata$METAL == "Platinum","proj_name"] <- "HN_P320160"
		
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2016 & rsdata$METAL == "Bronze","proj_name"] <- "HN_BR20160"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2016 & rsdata$METAL == "Silver","proj_name"] <- "HN_SIL20160"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2016 & rsdata$METAL == "Gold","proj_name"] <- "HN_G20160"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2016 & rsdata$METAL == "Platinum","proj_name"] <- "HN_P20160"
		
		#rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2016,"proj_name"] <- "HN_320160"
		#rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2016,"proj_name"] <- "HN_20160"
		
	# 2015:
		# Anthem: No consolidation needed
		# Blue Shield: No consolidation needed
		# Health Net: consolidate by plan type across markets and metals
		# Kaiser: No consolidation needed
		
		# Health Net
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2015 & rsdata$METAL == "Bronze","proj_name"] <- "HN_BR320150"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2015 & rsdata$METAL == "Silver","proj_name"] <- "HN_SIL320150"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2015 & rsdata$METAL == "Gold","proj_name"] <- "HN_G320150"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2015 & rsdata$METAL == "Platinum","proj_name"] <- "HN_P320150"
		
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2015 & rsdata$METAL == "Bronze","proj_name"] <- "HN_BR20150"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2015 & rsdata$METAL == "Silver","proj_name"] <- "HN_SIL20150"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2015 & rsdata$METAL == "Gold","proj_name"] <- "HN_G20150"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2015 & rsdata$METAL == "Platinum","proj_name"] <- "HN_P20150"
		
	# 2014:
		# Anthem: Consolidate plans by metal and network type across markets
		# Blue Shield: No consolidation needed
		# Health Net: Consolidate plans by metal and network type across markets
		# Kaiser: No consolidation needed
		
		# Anthem
		rsdata[rsdata$Anthem == 1 & rsdata$HMO == 1 & rsdata$year == 2014 & rsdata$METAL == "Silver","proj_name"] <- "ANT_SIL320140"
		rsdata[rsdata$Anthem == 1 & rsdata$HMO == 1 & rsdata$year == 2014 & rsdata$METAL == "Gold","proj_name"] <- "ANT_G320140"
		rsdata[rsdata$Anthem == 1 & rsdata$HMO == 1 & rsdata$year == 2014 & rsdata$METAL == "Platinum","proj_name"] <- "ANT_P320140"
		
		rsdata[rsdata$Anthem == 1 & rsdata$HMO == 0 & rsdata$year == 2014 & rsdata$METAL == "Bronze","proj_name"] <- "ANT_BR20140"
		rsdata[rsdata$Anthem == 1 & rsdata$HMO == 0 & rsdata$year == 2014 & rsdata$METAL == "Silver","proj_name"] <- "ANT_SIL20140"
		rsdata[rsdata$Anthem == 1 & rsdata$HMO == 0 & rsdata$year == 2014 & rsdata$METAL == "Gold","proj_name"] <- "ANT_G20140"
		rsdata[rsdata$Anthem == 1 & rsdata$HMO == 0 & rsdata$year == 2014 & rsdata$METAL == "Platinum","proj_name"] <- "ANT_P20140"
		
		
		# Health Net
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2014 & rsdata$METAL == "Silver","proj_name"] <- "HN_SIL320140"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2014 & rsdata$METAL == "Gold","proj_name"] <- "HN_G320140"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 1 & rsdata$year == 2014 & rsdata$METAL == "Platinum","proj_name"] <- "HN_P320140"
		
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2014 & rsdata$METAL == "Bronze","proj_name"] <- "HN_BR20140"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2014 & rsdata$METAL == "Silver","proj_name"] <- "HN_SIL20140"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2014 & rsdata$METAL == "Gold","proj_name"] <- "HN_G20140"
		rsdata[rsdata$Health_Net == 1 & rsdata$HMO == 0 & rsdata$year == 2014 & rsdata$METAL == "Platinum","proj_name"] <- "HN_P20140"
		
	#### Now we build the predicted cost object to use for predicted cost regressions/estimation

	projdata <- rsdata[!duplicated(rsdata$proj_name),
		c("proj_name","pmt_name","plan_name","region","year","insurer_small","METAL","AV_METAL","HMO","AV","Silver","Gold","Platinum",
			"Anthem","Blue_Shield","Health_Net","Kaiser","risk_score","demand","predicted_net_cost","predicted_reins_factor","model_predicted_claims")]
	rownames(projdata) <- projdata$proj_name
	
	projdata$PRJ_MM <- by(rsdata$PRJ_MM,rsdata$proj_name,sum,na.rm=T)[rownames(projdata)]
	projdata$PRJ_TP <- by(rsdata$PRJ_TP,rsdata$proj_name,sum,na.rm=T)[rownames(projdata)]
	projdata$PRJ_INC_CLM <- by(rsdata$PRJ_INC_CLM,rsdata$proj_name,sum,na.rm=T)[rownames(projdata)]
	projdata$PRJ_RSK_ADJ <- by(rsdata$PRJ_RSK_ADJ,rsdata$proj_name,sum,na.rm=T)[rownames(projdata)]
	projdata$PRJ_REIN <- by(rsdata$PRJ_REIN,rsdata$proj_name,sum,na.rm=T)[rownames(projdata)]
	projdata$PRJ_INC_CLM_PMPM <- projdata$PRJ_INC_CLM/projdata$PRJ_MM
	projdata$PRJ_RSK_ADJ_PMPM <- projdata$PRJ_RSK_ADJ/projdata$PRJ_MM
	projdata$PRJ_REIN_PMPM <- projdata$PRJ_REIN/projdata$PRJ_MM
		
	# Fix regions
	projdata[projdata$Anthem == 1 & projdata$year == 2014,"region"] <- 0
	projdata[projdata$Blue_Shield == 1 & projdata$year >= 2017,"region"] <- 0
	projdata[projdata$Health_Net == 1,"region"] <- 0
	
	# Assign Health Nets which were consolidated across metals silver
	projdata[projdata$Health_Net & projdata$year >= 2015,"METAL"] <- "Silver" 
	
##### Save and output data

	if(year == 2018) {
		keep_fields <- c("pmt_name","proj_name","plan_name","year","region","log_cost","AV_Demean","Silver","Gold","Platinum","METAL",
			"predicted_risk_score","log_risk_score","predicted_net_cost","predicted_reins_factor","actual_net_cost",
			"Anthem","Blue_Shield","Health_Net","Kaiser","HMO","trend","EXP_MM","PRJ_MM",
			"share_18to34","share_35to54","share_250to400","share_gt400",
			"share_male","share_SEP","share_Hispanic",
			"share_ra2","share_ra3","share_ra4","share_ra5","share_ra6","share_ra7", 
			"share_ra8","share_ra9","share_ra10","share_ra11","share_ra12","share_ra13",
			"share_ra14","share_ra15","share_ra16","share_ra17","share_ra18","share_ra19")
		
		filename = paste("data/final/moments_data.csv",sep="")
		write.csv(rsdata[,keep_fields],filename)
	}
	