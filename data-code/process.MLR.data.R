# Load Data

	# 2014 Data
	finsum_to_keep <- c("ï..MR_SUBMISSION_TEMPLATE_ID","ROW_LOOKUP_CODE",
					"CMM_INDIVIDUAL_YEARLY","CMM_INDIVIDUAL_Q1",
					"CMM_INDIVIDUAL_DEFERRED_PY","CMM_INDIVIDUAL_DEFERRED_CY","CMM_INDIVIDUAL_TOTAL","year")
	mlr_to_keep <- c("ï..MR_SUBMISSION_TEMPLATE_ID","ROW_LOOKUP_CODE",
					"CMM_INDIVIDUAL_PY2","CMM_INDIVIDUAL_PY1","CMM_INDIVIDUAL_CY",
					"CMM_INDIVIDUAL_TOTAL","year")
	
	ins_data2014 <- read.csv("data/MLR/2014/MR_Submission_Template_Header.csv",header=TRUE)
	ins_data2014$year <- 2014
	ins_data2014 <- ins_data2014[ins_data2014$BUSINESS_STATE == "California",]
	
	finsum_data2014 <- read.csv("data/MLR/2014/Part1_2_Summary_Data_Premium_Claims.csv",header=TRUE)
	finsum_data2014$year <- 2014
	finsum_data2014 <- finsum_data2014[,finsum_to_keep]
	
	mlr_data2014 <- read.csv("data/MLR/2014/Part3_MLR_Rebate_Calculation.csv",header=TRUE)
	mlr_data2014$year <- 2014
	mlr_data2014 <- mlr_data2014[,mlr_to_keep]
	gc()
	
	# 2015 data
	ins_data2015 <- read.csv("data/MLR/2015/MR_Submission_Template_Header.csv",header=TRUE)
	ins_data2015$year <- 2015
	ins_data2015 <- ins_data2015[ins_data2015$BUSINESS_STATE == "California",]
	
	finsum_data2015 <- read.csv("data/MLR/2015/Part1_2_Summary_Data_Premium_Claims.csv",header=TRUE)
	finsum_data2015$year <- 2015
	finsum_data2015 <- finsum_data2015[,finsum_to_keep]
	
	mlr_data2015 <- read.csv("data/MLR/2015/Part3_MLR_Rebate_Calculation.csv",header=TRUE)
	mlr_data2015$year <- 2015
	mlr_data2015 <- mlr_data2015[,mlr_to_keep]
	gc()
	
	# 2016 data
	ins_data2016 <- read.csv("data/MLR/2016/MR_Submission_Template_Header.csv",header=TRUE)
	ins_data2016$year <- 2016
	ins_data2016 <- ins_data2016[ins_data2016$BUSINESS_STATE == "California",]
	
	finsum_data2016 <- read.csv("data/MLR/2016/Part1_2_Summary_Data_Premium_Claims.csv",header=TRUE)
	finsum_data2016$year <- 2016
	finsum_data2016 <- finsum_data2016[,finsum_to_keep]
	
	mlr_data2016 <- read.csv("data/MLR/2016/Part3_MLR_Rebate_Calculation.csv",header=TRUE)
	mlr_data2016$year <- 2016
	mlr_data2016 <- mlr_data2016[,mlr_to_keep]
	gc()
	
	# 2017 data
	ins_data2017 <- read.csv("data/MLR/2017/MR_Submission_Template_Header.csv",header=TRUE)
	ins_data2017$year <- 2017
	ins_data2017 <- ins_data2017[ins_data2017$BUSINESS_STATE == "CA",]
	
	ins_data2017[,"ï..MR_SUBMISSION_TEMPLATE_ID"] <- ins_data2017[,"MR_SUBMISSION_TEMPLATE_ID"]
	ins_data2017[,"MR_SUBMISSION_TEMPLATE_ID"] <- NULL
	ins_data2017 <- ins_data2017[,colnames(ins_data2016)]
	
	finsum_data2017 <- read.csv("data/MLR/2017/Part1_2_Summary_Data_Premium_Claims.csv",header=TRUE)
	finsum_data2017$year <- 2017
	finsum_data2017[,"ï..MR_SUBMISSION_TEMPLATE_ID"] <- finsum_data2017[,"MR_SUBMISSION_TEMPLATE_ID"]
	finsum_data2017[,"MR_SUBMISSION_TEMPLATE_ID"] <- NULL
	finsum_data2017 <- finsum_data2017[,finsum_to_keep]
	
	mlr_data2017 <- read.csv("data/MLR/2017/Part3_MLR_Rebate_Calculation.csv",header=TRUE)
	mlr_data2017$year <- 2017
	mlr_data2017[,"ï..MR_SUBMISSION_TEMPLATE_ID"] <- mlr_data2017[,"MR_SUBMISSION_TEMPLATE_ID"]
	mlr_data2017[,"MR_SUBMISSION_TEMPLATE_ID"] <- NULL
	mlr_data2017 <- mlr_data2017[,mlr_to_keep]
	gc()
	
	# 2018 data
	ins_data2018 <- read.csv("data/MLR/2018/MR_Submission_Template_Header.csv",header=TRUE)
	ins_data2018$year <- 2018
	ins_data2018 <- ins_data2018[ins_data2018$BUSINESS_STATE == "CA",]
	
	ins_data2018[,"ï..MR_SUBMISSION_TEMPLATE_ID"] <- ins_data2018[,"MR_SUBMISSION_TEMPLATE_ID"]
	ins_data2018[,"MR_SUBMISSION_TEMPLATE_ID"] <- NULL
	ins_data2018 <- ins_data2018[,colnames(ins_data2016)]
	
	finsum_data2018 <- read.csv("data/MLR/2018/Part1_2_Summary_Data_Premium_Claims.csv",header=TRUE)
	finsum_data2018$year <- 2018
	finsum_data2018[,"ï..MR_SUBMISSION_TEMPLATE_ID"] <- finsum_data2018[,"MR_SUBMISSION_TEMPLATE_ID"]
	finsum_data2018[,"MR_SUBMISSION_TEMPLATE_ID"] <- NULL
	finsum_data2018 <- finsum_data2018[,finsum_to_keep]
	
	mlr_data2018 <- read.csv("data/MLR/2018/Part3_MLR_Rebate_Calculation.csv",header=TRUE)
	mlr_data2018$year <- 2018
	mlr_data2018[,"ï..MR_SUBMISSION_TEMPLATE_ID"] <- mlr_data2018[,"MR_SUBMISSION_TEMPLATE_ID"]
	mlr_data2018[,"MR_SUBMISSION_TEMPLATE_ID"] <- NULL
	mlr_data2018 <- mlr_data2018[,mlr_to_keep]
	gc()
	
	
	
	# Risk adjustment/reinsurance data
	
		### Update Risk corridors
		
	ra_reins <- read.csv("data/ra_reins.csv",header=TRUE)
	ra_reins <- ra_reins[ra_reins$STATE == "CA",]
	#rc <- read.csv("rc.csv",header=TRUE)
	
	
	# Merge Insurer Data

		rownames(ins_data2014) <- ins_data2014[,1]
		rownames(ins_data2015) <- ins_data2015[,1]
		rownames(ins_data2016) <- ins_data2016[,1]
		rownames(ins_data2017) <- ins_data2017[,1]
		rownames(ins_data2018) <- ins_data2018[,1]
		
		data <- rbind(ins_data2014,ins_data2015,ins_data2016,ins_data2017,ins_data2018)
		rm(ins_data2014)
		rm(ins_data2015)
		rm(ins_data2016)
		rm(ins_data2017)
		rm(ins_data2018)
		
		to.delete <- c("ï..MR_SUBMISSION_TEMPLATE_ID","DOMICILIARY_STATE","COMPANY_ADDRESS",
			"CREATED_DATE" )
		data <- data[,!colnames(data) %in% to.delete]
		gc()
		
	# Merge Financial Summary Data

		rownames(finsum_data2014) <- paste(finsum_data2014[,1],finsum_data2014[,2],sep="_")
		rownames(finsum_data2015) <- paste(finsum_data2015[,1],finsum_data2015[,2],sep="_")
		rownames(finsum_data2016) <- paste(finsum_data2016[,1],finsum_data2016[,2],sep="_")
		rownames(finsum_data2017) <- paste(finsum_data2017[,1],finsum_data2017[,2],sep="_")
		rownames(finsum_data2018) <- paste(finsum_data2018[,1],finsum_data2018[,2],sep="_")
		
		finsum_data <- rbind(finsum_data2014,finsum_data2015,finsum_data2016,finsum_data2017,finsum_data2018)
		rm(finsum_data2014)
		rm(finsum_data2015)
		rm(finsum_data2016)
		rm(finsum_data2017)
		rm(finsum_data2018)
	
	# Merge MLR Data

		rownames(mlr_data2014) <- paste(mlr_data2014[,1],mlr_data2014[,2],sep="_")
		rownames(mlr_data2015) <- paste(mlr_data2015[,1],mlr_data2015[,2],sep="_")
		rownames(mlr_data2016) <- paste(mlr_data2016[,1],mlr_data2016[,2],sep="_")
		rownames(mlr_data2017) <- paste(mlr_data2017[,1],mlr_data2017[,2],sep="_")
		rownames(mlr_data2018) <- paste(mlr_data2018[,1],mlr_data2018[,2],sep="_")
		
		mlr_data <- rbind(mlr_data2014,mlr_data2015,mlr_data2016,mlr_data2017,mlr_data2018)
		rm(mlr_data2014)
		rm(mlr_data2015)
		rm(mlr_data2016)
		rm(mlr_data2017)
		rm(mlr_data2018)
	
# Pull Financial Data

	data$Member_months <- finsum_data[paste(rownames(data),"MEMBER_MONTHS",sep="_"),"CMM_INDIVIDUAL_Q1"] # Part 1, Line 7.4
	data$Premiums <- finsum_data[paste(rownames(data),"TOTAL_DIRECT_PREMIUM_EARNED",sep="_"),"CMM_INDIVIDUAL_Q1"] # Part 1, Line 1.1
	data$Claims <- finsum_data[paste(rownames(data),"TOTAL_INCURRED_CLAIMS_PT1",sep="_"),"CMM_INDIVIDUAL_Q1"] # Part 1, Line 2.1
	
	na_to_zero <- function(finsum_data,fields) {
		for (i in fields) {
			missing_rows <- is.na(finsum_data[paste(rownames(data),i,sep="_"),"CMM_INDIVIDUAL_Q1"])
			if(length(missing_rows) > 0) {
				finsum_data[paste(rownames(data),i,sep="_"),"CMM_INDIVIDUAL_Q1"][missing_rows] <- 0
			}
		}
		return(finsum_data)
	}
	
	fields <- c("PCORI_FEE","ACA_9010_FEE","FED_REINS_CONTRIBUTIONS","STATE_PREMIUM_TAXES")
	finsum_data <- na_to_zero(finsum_data,fields)
	
	data$ACA_taxes_fees <- finsum_data[paste(rownames(data),"PCORI_FEE",sep="_"),"CMM_INDIVIDUAL_Q1"] + 
							finsum_data[paste(rownames(data),"ACA_9010_FEE",sep="_"),"CMM_INDIVIDUAL_Q1"] + 
							finsum_data[paste(rownames(data),"FED_REINS_CONTRIBUTIONS",sep="_"),"CMM_INDIVIDUAL_Q1"] + 
							finsum_data[paste(rownames(data),"STATE_PREMIUM_TAXES",sep="_"),"CMM_INDIVIDUAL_Q1"] 
	
	fields <- c("FED_INCOME_TAX_DEDUCTIBLE_PREM","OTHER_FED_TAXES_AND_ASSESSMENTS","STATE_INCOME_EXCISE_BUSINES_OTH",
		"REG_AUTHORITY_LICENSES_AND_FEES","COMMUNITY_BENEFIT_EXPENDITURES","STATE_TAXES_ASSMTS_NOT_EXC_PREM",
		"FINES_PENLTS_OF_REG_AUTHORITIES")
	finsum_data <- na_to_zero(finsum_data,fields)
	
	data$FDSTLCL_taxes_fees <- finsum_data[paste(rownames(data),"FED_INCOME_TAX_DEDUCTIBLE_PREM",sep="_"),"CMM_INDIVIDUAL_Q1"] + 
							finsum_data[paste(rownames(data),"OTHER_FED_TAXES_AND_ASSESSMENTS",sep="_"),"CMM_INDIVIDUAL_Q1"] + 
							finsum_data[paste(rownames(data),"STATE_INCOME_EXCISE_BUSINES_OTH",sep="_"),"CMM_INDIVIDUAL_Q1"] + 
							finsum_data[paste(rownames(data),"REG_AUTHORITY_LICENSES_AND_FEES",sep="_"),"CMM_INDIVIDUAL_Q1"] +
							finsum_data[paste(rownames(data),"COMMUNITY_BENEFIT_EXPENDITURES",sep="_"),"CMM_INDIVIDUAL_Q1"]
							
	fields <- c("IMPROVE_HEALTH_OUTCOMES","ACTIVITES_TO_PREVENT_HOSP_READM","IMP_PAT_SAFETY_REDUCE_MED_ERRS",
				"WELLNESS_AND_HEALTH_PROM_ACTS","HITER_TO_HEALTH_IMPROVEMENT","ALLOWABLE_ICD10_EXPENSES",
				"COST_CONTAINMENT_EXP_NOT_INCL","ALL_OTHER_CLAIMS_ADJ_EXPENSES","DIR_SALES_SALARIES_AND_BENEFITS",
				"AGNTS_AND_BROKERS_FEES_COMMS","OTHER_GENERAL_AND_ADM_EXPENSES")
	finsum_data <- na_to_zero(finsum_data,fields)
	
	data$Admin_costs <- finsum_data[paste(rownames(data),"IMPROVE_HEALTH_OUTCOMES",sep="_"),"CMM_INDIVIDUAL_Q1"] + 
							finsum_data[paste(rownames(data),"ACTIVITES_TO_PREVENT_HOSP_READM",sep="_"),"CMM_INDIVIDUAL_Q1"] + 
							finsum_data[paste(rownames(data),"IMP_PAT_SAFETY_REDUCE_MED_ERRS",sep="_"),"CMM_INDIVIDUAL_Q1"] + 
							finsum_data[paste(rownames(data),"WELLNESS_AND_HEALTH_PROM_ACTS",sep="_"),"CMM_INDIVIDUAL_Q1"] +
							finsum_data[paste(rownames(data),"HITER_TO_HEALTH_IMPROVEMENT",sep="_"),"CMM_INDIVIDUAL_Q1"] + 
							finsum_data[paste(rownames(data),"ALLOWABLE_ICD10_EXPENSES",sep="_"),"CMM_INDIVIDUAL_Q1"] + 
							finsum_data[paste(rownames(data),"COST_CONTAINMENT_EXP_NOT_INCL",sep="_"),"CMM_INDIVIDUAL_Q1"] +
							finsum_data[paste(rownames(data),"ALL_OTHER_CLAIMS_ADJ_EXPENSES",sep="_"),"CMM_INDIVIDUAL_Q1"] + 
							finsum_data[paste(rownames(data),"DIR_SALES_SALARIES_AND_BENEFITS",sep="_"),"CMM_INDIVIDUAL_Q1"] + 
							finsum_data[paste(rownames(data),"OTHER_GENERAL_AND_ADM_EXPENSES",sep="_"),"CMM_INDIVIDUAL_Q1"]  

	data$Commissions <- finsum_data[paste(rownames(data),"AGNTS_AND_BROKERS_FEES_COMMS",sep="_"),"CMM_INDIVIDUAL_Q1"]

	data$Var_Admin_Low <- data$ACA_taxes_fees + data$FDSTLCL_taxes_fees + data$Admin_costs -
							finsum_data[paste(rownames(data),"OTHER_GENERAL_AND_ADM_EXPENSES",sep="_"),"CMM_INDIVIDUAL_Q1"] - 
							finsum_data[paste(rownames(data),"DIR_SALES_SALARIES_AND_BENEFITS",sep="_"),"CMM_INDIVIDUAL_Q1"]
	
	data$Var_Admin_High <- data$ACA_taxes_fees + data$FDSTLCL_taxes_fees + data$Admin_costs -
							finsum_data[paste(rownames(data),"DIR_SALES_SALARIES_AND_BENEFITS",sep="_"),"CMM_INDIVIDUAL_Q1"]
			
	data$Fixed_Admin_Low <- finsum_data[paste(rownames(data),"OTHER_GENERAL_AND_ADM_EXPENSES",sep="_"),"CMM_INDIVIDUAL_Q1"] + 
							finsum_data[paste(rownames(data),"DIR_SALES_SALARIES_AND_BENEFITS",sep="_"),"CMM_INDIVIDUAL_Q1"]
	
	data$Fixed_Admin_High <- finsum_data[paste(rownames(data),"DIR_SALES_SALARIES_AND_BENEFITS",sep="_"),"CMM_INDIVIDUAL_Q1"]
	
	
# Pull MLR Data

	data$MLR <- mlr_data[paste(rownames(data),"CREDIBILITY_ADJUSTED_MLR_LN4_4",sep="_"),"CMM_INDIVIDUAL_TOTAL"] 
	data$MLR_Rebate <- mlr_data[paste(rownames(data),"REBATE_AMT_CREDIBILITY_ADJ_MLR",sep="_"),"CMM_INDIVIDUAL_TOTAL"] 
	
	data$Risk_adjustment_rec <- mlr_data[paste(rownames(data),"FED_RISK_ADJ_NET_PAYMENTS_HHS",sep="_"),"CMM_INDIVIDUAL_CY"] 
	data$Reinsurance_rec <- mlr_data[paste(rownames(data),"FED_REINS_PAYMENTS_HHS",sep="_"),"CMM_INDIVIDUAL_CY"] 
	data$Risk_corridor_rec <- mlr_data[paste(rownames(data),"FED_RISK_CORR_NET_PAYMENTS_HHS",sep="_"),"CMM_INDIVIDUAL_CY"]					
				
	# Some insurers defer all or part of their 3R payments			
				
	data$RA_defer_flag <- finsum_data[paste(rownames(data),"FED_RISK_ADJ_NET_PAYMENTS",sep="_"),"CMM_INDIVIDUAL_DEFERRED_CY"]
	data$RA_defer_flag <- data$RA_defer_flag > 0 & !is.na(data$RA_defer_flag)
	data[data$RA_defer_flag,"Risk_adjustment_rec"] <- 
		finsum_data[paste(rownames(data[data$RA_defer_flag,]),"FED_RISK_ADJ_NET_PAYMENTS",sep="_"),"CMM_INDIVIDUAL_Q1"]
	
	data$REINS_defer_flag <- finsum_data[paste(rownames(data),"FED_REINS_PAYMENTS",sep="_"),"CMM_INDIVIDUAL_DEFERRED_CY"]
	data$REINS_defer_flag <- data$REINS_defer_flag > 0 & !is.na(data$REINS_defer_flag)
	data[data$REINS_defer_flag,"Reinsurance_rec"] <- 
		finsum_data[paste(rownames(data[data$REINS_defer_flag,]),"FED_REINS_PAYMENTS",sep="_"),"CMM_INDIVIDUAL_Q1"]
	
	data$RC_defer_flag <- finsum_data[paste(rownames(data),"FED_RISK_CORR_NET_PAYMENTS",sep="_"),"CMM_INDIVIDUAL_DEFERRED_CY"]
	data$RC_defer_flag <- data$RC_defer_flag != 0 & !is.na(data$RC_defer_flag)
	data[data$RC_defer_flag,"Risk_corridor_rec"] <- 
		finsum_data[paste(rownames(data[data$RC_defer_flag,]),"FED_RISK_CORR_NET_PAYMENTS",sep="_"),"CMM_INDIVIDUAL_Q1"]

	data <- data[,!colnames(data) %in% c("RA_defer_flag","REINS_defer_flag","RC_defer_flag")]
		
		
	#data$Risk_adjustment_rec <- finsum_data[paste(rownames(data),"FED_RISK_ADJ_NET_PAYMENTS",sep="_"),"CMM_INDIVIDUAL_Q1"] # Part 2, Line 1.10
	#data$Reinsurance_rec <- finsum_data[paste(rownames(data),"FED_REINS_PAYMENTS",sep="_"),"CMM_INDIVIDUAL_Q1"] # Part 2, line 1.9
	#data$Risk_corridor_rec <- finsum_data[paste(rownames(data),"FED_RISK_CORR_NET_PAYMENTS",sep="_"),"CMM_INDIVIDUAL_Q1"] # Part 2 Line 1.11

# Drop Insurers not in individual market	
	
	# Drop insurers with missing premiums or claims and not receiving risk adjustment
	data <- data[!is.na(data$Premiums),]
	data <- data[!is.na(data$Claims),]
	
	# Drop insurers with zero premiums or zero claims
	data <- data[!data$Premiums == 0,]
	data <- data[!data$Claims == 0,]
	
	
	# Drop insurers with no members
	data <- data[!(data$Member_months == 0 & !is.na(data$Member_months)),]

	
	# Drop all insurers that have NA for both risk adjustment and reinsurance (only grand total states have one or the other)
		# These insurers are not in single risk pool
		# For some reason, Anthem 2018 (27603) is not populated
		
	data <- data[!(is.na(data$Reinsurance_rec) | is.na(data$Risk_adjustment_rec)) | data$HIOS_ISSUER_ID == 27603,]
	
	# Drop all insurers that have 0 for both risk adjustment and reinsurance
		# These insurers are not in single risk pool
	
	data <- data[!(data$Reinsurance_rec == 0 & data$Risk_adjustment_rec == 0),]
	
	# Drop insurer if HIOS id not in RA/RI - This only affects United in 2014-15,2017

	data <- data[data$HIOS_ISSUER_ID %in% ra_reins$HIOS,]
	
	
##### Check to make sure risk adjustment and reinsurance amounts are correct
	
		# Drop insurers with no risk adjustment payments
		ra_reins <- ra_reins[ra_reins$RA_IND != 0,]
		rownames(ra_reins) <- paste(ra_reins$HIOS,ra_reins$Year,sep="_")
	
		# Drop insurer-year combinations not in RA/RI file 
		data <- data[paste(data$HIOS_ISSUER_ID,data$year,sep="_") %in% rownames(ra_reins),]
	
		data$Risk_adjustment_rec <- ra_reins[paste(data$HIOS_ISSUER_ID,data$year,sep="_"),"RA_IND"]
		data$Reinsurance_rec <- ra_reins[paste(data$HIOS_ISSUER_ID,data$year,sep="_"),"Reinsurance"]
			
	# Take 3 R's out of premiums
	data$Premiums_w3Rs <- data$Premiums
	data$Premiums <- finsum_data[paste(rownames(data),"DIRECT_PREMIUM_WRITTEN",sep="_"),"CMM_INDIVIDUAL_Q1"] # Part 2, Line 1.1
		
##### Combine Small Exchange Insurers: LA Care, Molina, Valley, Oscar, United, Sharp, Western, Contra Costa		
	
	
	data$COMPANY_NAME <- as.character(data$COMPANY_NAME)
	data$GROUP_AFFILIATION <- as.character(data$GROUP_AFFILIATION)

	small_exchange_insurers <- c("County of Santa Clara","Chinese Community Health Plan",
		"CONTRA COSTA MEDICAL SERVICES DBA CONTRA COSTA HEALTH PLAN",
        "Local Initiative Health Authority for Los Angeles County",
		"MOLINA HEALTHCARE OF CALIFORNIA","Oscar Health Plan of California",
		"Sharp Health Plan","UnitedHealthcare Benefits Plan of California","Western Health Advantage") 
		
	years <- c(2014:2018)		
	for (t in years) {	
		data <- rbind(data,NA)	
		data[nrow(data),"BUSINESS_STATE"] <- "CA"
		data[nrow(data),"GROUP_AFFILIATION"] <- "Small_Insurer"
		data[nrow(data),"COMPANY_NAME"] <- "Small_Insurer"
		data[nrow(data),"DBA_MARKETING_NAME "] <- "Small_Insurer"
		data[nrow(data),"year"] <- t
		data[nrow(data),"Member_months"] <- sum(data[data$year == t & data$COMPANY_NAME %in% small_exchange_insurers,"Member_months"],na.rm=TRUE)
		data[nrow(data),"Premiums"] <- sum(data[data$year == t & data$COMPANY_NAME %in% small_exchange_insurers,"Premiums"],na.rm=TRUE)
		data[nrow(data),"Claims"] <- sum(data[data$year == t & data$COMPANY_NAME %in% small_exchange_insurers,"Claims"],na.rm=TRUE)
		data[nrow(data),"ACA_taxes_fees"] <- sum(data[data$year == t & data$COMPANY_NAME %in% small_exchange_insurers,"ACA_taxes_fees"],na.rm=TRUE)
		data[nrow(data),"FDSTLCL_taxes_fees"] <- sum(data[data$year == t & data$COMPANY_NAME %in% small_exchange_insurers,"FDSTLCL_taxes_fees"],na.rm=TRUE)
		data[nrow(data),"Admin_costs"] <- sum(data[data$year == t & data$COMPANY_NAME %in% small_exchange_insurers,"Admin_costs"],na.rm=TRUE)
		data[nrow(data),"Var_Admin_Low"] <- sum(data[data$year == t & data$COMPANY_NAME %in% small_exchange_insurers,"Var_Admin_Low"],na.rm=TRUE)
		data[nrow(data),"Var_Admin_High"] <- sum(data[data$year == t & data$COMPANY_NAME %in% small_exchange_insurers,"Var_Admin_High"],na.rm=TRUE)
		data[nrow(data),"Fixed_Admin_Low"] <- sum(data[data$year == t & data$COMPANY_NAME %in% small_exchange_insurers,"Fixed_Admin_Low"],na.rm=TRUE)
		data[nrow(data),"Fixed_Admin_High"] <- sum(data[data$year == t & data$COMPANY_NAME %in% small_exchange_insurers,"Fixed_Admin_High"],na.rm=TRUE)
		data[nrow(data),"MLR_Rebate"] <- sum(data[data$year == t & data$COMPANY_NAME %in% small_exchange_insurers,"MLR_Rebate"],na.rm=TRUE)
		data[nrow(data),"Risk_adjustment_rec"] <- sum(data[data$year == t & data$COMPANY_NAME %in% small_exchange_insurers,"Risk_adjustment_rec"],na.rm=TRUE)
		data[nrow(data),"Reinsurance_rec"] <- sum(data[data$year == t & data$COMPANY_NAME %in% small_exchange_insurers,"Reinsurance_rec"],na.rm=TRUE)
		data[nrow(data),"Risk_corridor_rec"] <- sum(data[data$year == t & data$COMPANY_NAME %in% small_exchange_insurers,"Risk_corridor_rec"],na.rm=TRUE)
		data[nrow(data),"Premiums_w3Rs"] <- sum(data[data$year == t & data$COMPANY_NAME %in% small_exchange_insurers,"Premiums_w3Rs"],na.rm=TRUE)
	}
	
write.csv(data,"data/final/mlr_data.csv")	
	