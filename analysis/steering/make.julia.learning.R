setwd("C:/Users/esaltzm/OneDrive - Emory University/Google Drive/Emory/Research/Navigators/Data")  # Office computer directory
choices <- get(load("ca_choice_matrix_FEB052021_small"))
previous_choices <- get(load("ca_previous_choice_matrix_FEB052021_small"))	
premiums <- get(load("ca_premium_matrix_FEB052021_small"))
commissions <- get(load("ca_commissions_matrix_FEB052021_small"))
renewals <- get(load("ca_renewals_matrix_FEB052021_small"))
subsidies <- get(load("ca_subsidy_matrix_FEB052021_small"))
households <- get(load("ca_household_characteristics_FEB052021_small"))
	
##### Initialize output object

	valid_choices <- as.vector(!is.na(t(choices)))	
	julia_output <- expand.grid(colnames(choices),1:nrow(choices))[valid_choices,]
	gc()
	
##### Save year and weight from household object and then delete
	
	household_years <- rep(households[rownames(choices),"year"],each=ncol(choices))[valid_choices]	
	weight <- rep(households[rownames(choices),"weight"],each=ncol(choices))[valid_choices]
	penalties <- rep(households[rownames(choices),"penalty"]/12,each=ncol(choices))[valid_choices]
	#rm(households)
	gc()
		
##### Reduce memory of premium and choice objects
	
	num_households <- nrow(choices)
	num_plans <- ncol(choices)
	
	# Reduce memory
	choices <- as.vector(t(choices))[valid_choices]
	gc()
	
	previous_choices <- as.vector(t(previous_choices))[valid_choices]
	gc()
	
	premiums <- as.vector(t(premiums))[valid_choices] - penalties
	gc()
	
	commissions <- as.vector(t(commissions))[valid_choices]
	gc()
	
	renewals <- as.vector(t(renewals))[valid_choices]
	gc()
	
	subsidies <- as.vector(t(subsidies))[valid_choices]
	gc()
	
##### To send to Julia
	
	julia_output$choice <- choices
	rm(choices)
	gc()
	
	julia_output$previous_choice <- previous_choices
	julia_output[is.na(julia_output$previous_choice),"previous_choice"] <- 0
	
	julia_output$premium <- premiums
	julia_output$subsidy <- subsidies
	julia_output$penalty <- penalties
	julia_output$commission <- commissions
	julia_output$renewal <- renewals
	rm(premiums)
	rm(subsidies)
	rm(penalties)
	gc()
	
	julia_output$year <- household_years
	rm(household_years)
	gc()
	
	julia_output$weight <- weight
	rm(weight)
	gc()
		
	julia_output$uninsured_plan <- rep(c(rep(0,num_plans-1),1),num_households)[valid_choices]
	julia_output[julia_output$uninsured_plan == 1,c("premium","subsidy")] <- 0
	
	colnames(julia_output) <- c("plan_name","household_number","choice","previous_choice",
		"premium","subsidy","penalty","commission","renewal","year","weight","uninsured_plan")
	write.csv(julia_output,file="ca_julia_data_FEB052021_small.csv",row.names=FALSE)		
	gc()
	
