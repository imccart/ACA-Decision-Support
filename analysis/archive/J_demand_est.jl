####################################################
### Estimate Demand Model for Covered California ###
####################################################
		
##### Load Data

	# Packages
	using DataFrames
	using StatsBase
	using Optim
	using Calculus
	using Distributions
	using Random
	using DelimitedFiles
	using CSV
	using LinearAlgebra
	using Libdl
	
##### Specify simulation parameters and variables
	
	choices = convert(Array,data[!,:choice]);
	household_numbers = convert(Array,data[!,:household_number]);	
			
	# Indicator for nested logit gradient and hessian
	I_ex_nest = zero(choices);
	unique_hh = unique(household_numbers);
	for id in unique_hh
		household_ids = findall(household_numbers .== id);
		I_ex_nest[household_ids] .= 1 - choices[household_ids[length(household_ids)]];
	end	
		
##### Form Covariate Matrixbe

	X = form_X_matrix(all_covariates, data, choices, uninsured_plans);
	
##### Estimation with Nelder Mead
	
	obj_nested_logit(betap) = objective_nested_logit(betap,X,choices);
	nested_logit = optimize(obj_nested_logit, beta_initial, iterations=50000, show_trace=true) ;				
#	nested_logit = optimize(obj_nested_logit, beta_initial, BFGS(); autodiff = :forward) ;
#	nested_logit = optimize(obj_nested_logit, g!, beta_initial, LBFGS()) ;
	min_beta = Optim.minimizer(nested_logit)
	
##### Output Processing

	# Calculate standard errors
	probs = simulate_nested_probabilities(min_beta,X);
	
	# Save output
	param_output = DataFrame();
	param_output[!,:Variable] = cat(all_covariates,"lambda"; dims=1);
	param_output[!,:Coef] = min_beta;
		
		
		
		