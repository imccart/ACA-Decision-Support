### Compute Logit Gradient and Hessian

		# Reference: p.18 of https://cran.r-project.org/web/packages/mlogit/vignettes/mlogit.pdf
		
		function compute_nested_logit_gradient(betap,X,choices,probs)
		
			lambda = betap[length(betap)];
			betap = betap[1:(length(betap)-1)];
			U = X * betap;
			
			inside_option_probs = zero(probs);
			ex_probs = zero(probs);
			N_exchange = zero(probs);
			
			for id in unique_hh
				household_ids = findall(household_numbers .== id);
				
				inside_option_prob = 1 - probs[household_ids[length(household_ids)]];
				inside_option_probs[household_ids] .= inside_option_prob;
				
				ex_probs[household_ids] = probs[household_ids]/inside_option_prob;
				ex_probs[household_ids[length(household_ids)]] = 0;
				
				N_exchange[household_ids] .= sum(exp.(U[household_ids[1:(length(household_ids)-1)]]/lambda)) ;
			end
			
			# Gradient wrt beta
			gradient_beta = (1/lambda) * (transpose(X) * choices) + 
							((lambda-1)/lambda) * (transpose(X) * (ex_probs .* I_ex_nest)) - 
							transpose(X) * probs;
			
			# Gradient wrt lambda (This should be a scalar!)
			gradient_lambda = -(1/lambda^2) * (transpose(U) * choices) + 
							transpose(choices .* (1 .- uninsured_plans)) * log.(N_exchange) + 
							((1 - lambda)/lambda^2) * (transpose(U) * (ex_probs .* I_ex_nest)) .- 
							transpose(choices .* inside_option_probs) * log.(N_exchange) + 
							(1/lambda) * (transpose(U) * probs);
		
			return(cat(dims=1,gradient_beta,gradient_lambda))
		end


		function g!(G,betap)
		
			probs = simulate_nested_probabilities(betap,X);
			lambda = betap[length(betap)];
			betap = betap[1:(length(betap)-1)];
			U = X * betap;
			
			inside_option_probs = zero(probs);
			ex_probs = zero(probs);
			N_exchange = zero(probs);
			
			for id in unique_hh
				household_ids = findall(household_numbers .== id);
				
				inside_option_prob = 1 - probs[household_ids[length(household_ids)]];
				inside_option_probs[household_ids] .= inside_option_prob;
				
				ex_probs[household_ids] = probs[household_ids]/inside_option_prob;
				ex_probs[household_ids[length(household_ids)]] = 0;
				
				N_exchange[household_ids] .= sum(exp.(U[household_ids[1:(length(household_ids)-1)]]/lambda)) ;
			end
			
			# Gradient wrt beta
			gradient_beta = (1/lambda) * (transpose(X) * choices) + 
							((lambda-1)/lambda) * (transpose(X) * (ex_probs .* I_ex_nest)) - 
							transpose(X) * probs;
			
			# Gradient wrt lambda (This should be a scalar!)
			gradient_lambda = -(1/lambda^2) * (transpose(U) * choices) + 
							transpose(choices .* (1 .- uninsured_plans)) * log.(N_exchange) + 
							((1 - lambda)/lambda^2) * (transpose(U) * (ex_probs .* I_ex_nest)) .- 
							transpose(choices .* inside_option_probs) * log.(N_exchange) + 
							(1/lambda) * (transpose(U) * probs);
		
			G[1:(length(betap))]=gradient_beta;
			G[length(betap)+1]=gradient_lambda
		end

		
		function compute_nested_logit_hessian(betap,X,choices,probs)
		
			min_beta = betap;
			lambda = betap[length(betap)];
			betap = betap[1:(length(betap)-1)];
			U = X * betap;
		
			hess = zeros(length(min_beta),length(min_beta));
			inside_option_probs = zero(probs);
			ex_probs = zero(probs);
			N_exchange = zero(probs);
			inner_sum = zero(X);
			ex_inner_sum = zero(X);
			utility_sum = zero(probs);
			utility_sum_uni = zero(probs);
			ex_utility_sum = zero(probs);
			ex_utility_sum_uni = zero(probs);
			
			for id in unique_hh
				household_ids = findall(household_numbers .== id);
				
				inside_option_prob = 1 - probs[household_ids[length(household_ids)]];
				inside_option_probs[household_ids] .= inside_option_prob;
				
				ex_probs[household_ids] = probs[household_ids]/inside_option_prob;
				ex_probs[household_ids[length(household_ids)]] = 0;
				
				N_exchange[household_ids] .= sum(exp.(U[household_ids[1:(length(household_ids)-1)]]/lambda)) ;
				
				utility_sum[household_ids,:] .= sum(probs[household_ids] .* U[household_ids]) ;
				utility_sum[household_ids[length(household_ids)]] = 0;
				utility_sum_uni[household_ids,:] .= sum(probs[household_ids] .* U[household_ids]) ;
				
				ex_utility_sum[household_ids,:] .= sum(ex_probs[household_ids] .* U[household_ids]) ;
				ex_utility_sum[household_ids[length(household_ids)]] = 0;
				ex_utility_sum_uni[household_ids,:] .= sum(ex_probs[household_ids] .* U[household_ids]) ;
				
				inner_sum[household_ids,:] = repeat(sum(probs[household_ids] .* X[household_ids,:],dims=1),outer=length(household_ids));
				ex_inner_sum[household_ids,:] = repeat(sum(ex_probs[household_ids] .* X[household_ids,:],dims=1),outer=length(household_ids));
			end
				
			for k in eachindex(min_beta), kp in eachindex(min_beta)
			
				if (k < length(min_beta) && kp < length(min_beta))
				
					partial_ex_prob_partial_beta = (1/lambda) * (ex_probs .* I_ex_nest) .* (X[:,kp] - ex_inner_sum[:,kp]);
					partial_prob_partial_beta = (1/lambda) * (X[:,kp] .* probs) + 
											(lambda - 1)/lambda * (probs .* ex_inner_sum[:,kp]) - 
											probs .* inner_sum[:,kp];
				
					hess[k,kp] = ((lambda - 1)/(lambda)) * sum(X[:,k] .* (partial_ex_prob_partial_beta .* I_ex_nest)) - 
								sum(X[:,k] .* partial_prob_partial_beta) ;					
				
				elseif (k < length(min_beta) || kp < length(min_beta))
				
					partial_ex_prob_partial_beta = (1/lambda) * (ex_probs .* I_ex_nest) .* (X[:,min(k,kp)] - ex_inner_sum[:,min(k,kp)]);
					partial_prob_partial_beta = (1/lambda) * (X[:,min(k,kp)] .* probs) + 
											(lambda - 1)/lambda * (probs .* ex_inner_sum[:,min(k,kp)]) - 
											probs .* inner_sum[:,min(k,kp)];
					partial_logNexchange_partial_beta = (1/lambda) * ex_inner_sum[:,min(k,kp)];
					partial_inside_prob_partial_beta = (1 .- inside_option_probs) .* inner_sum[:,min(k,kp)];
				
					hess[k,kp] = -(1/lambda^2) * sum(X[:,min(k,kp)] .* choices) +
								sum(choices .* (1 .- uninsured_plans) .* partial_logNexchange_partial_beta) + 
								((1 - lambda)/lambda^2) * sum(U .* (partial_ex_prob_partial_beta .* I_ex_nest) +
									X[:,min(k,kp)] .* (ex_probs .* I_ex_nest)) - 
								sum(choices .* partial_inside_prob_partial_beta .* log.(N_exchange) + 
									choices .* inside_option_probs .* partial_logNexchange_partial_beta) +
								(1/lambda) * sum(U .* partial_prob_partial_beta +
									X[:,min(k,kp)] .* probs) ;		
					
				else 
					
					partial_logNexchange_partial_lambda = -(1/lambda^2) * ex_utility_sum;
					partial_logNexchange_partial_lambda_uni = -(1/lambda^2) * ex_utility_sum_uni;
					partial_ex_prob_partial_lambda = -(1/lambda^2) * ex_probs .* (U - ex_utility_sum);
					partial_inside_prob_partial_lambda = -(1/lambda) * (1 .- inside_option_probs) .* (utility_sum) + 
						(1 .- inside_option_probs) .* inside_option_probs .* log.(N_exchange) ;
					partial_inside_prob_partial_lambda_uni = -(1/lambda) * (1 .- inside_option_probs) .* (utility_sum_uni) + 
						(1 .- inside_option_probs) .* inside_option_probs .* log.(N_exchange) ;
					partial_prob_partial_lambda = -(1/lambda^2) * (U .* probs) - ((lambda-1)/lambda^2) * probs .* ex_utility_sum + 
						(probs .* log.(N_exchange)) - probs .* (-1/lambda * utility_sum + inside_option_probs .* log.(N_exchange));
					
					hess[k,kp] = ((2/lambda^3) * (transpose(U) * choices) +
								sum((choices .* (1 .- uninsured_plans)) .* partial_logNexchange_partial_lambda) +
								((1 - lambda)/lambda^2) * (transpose(U) * (partial_ex_prob_partial_lambda .* I_ex_nest)) + 
									(lambda-2)/lambda^3 * (transpose(U) * (ex_probs .* I_ex_nest)) - 
								(transpose(choices .* partial_inside_prob_partial_lambda_uni) * log.(N_exchange) + 
									transpose(choices .* inside_option_probs) * partial_logNexchange_partial_lambda_uni) +
								(1/lambda) * (transpose(U) * partial_prob_partial_lambda) - 
									(1/lambda^2) * (transpose(U) * probs))[1];
					
				end

			end
			
			return(hess)
		end
		
	### Simulate Probabilities
	
		function simulate_nested_probabilities(betap,X) 

			lambda = betap[length(betap)];
			betap = betap[1:(length(betap)-1)];
			U = X * betap;
			probs = zero(U);
			
			for id in unique_hh
				household_ids = findall(household_numbers .== id);
				exchange_household_ids = household_ids[1:(length(household_ids)-1)];
				uninsured_household_id = household_ids[length(household_ids)];
				
				probs[exchange_household_ids] = (exp.(U[exchange_household_ids]/lambda) * (sum(exp.(U[exchange_household_ids]/lambda)))^(lambda - 1)) ./ 
					(1 + (sum(exp.(U[exchange_household_ids]/lambda)))^(lambda));
				probs[uninsured_household_id] = 1 / (1 + (sum(exp.(U[exchange_household_ids]/lambda)))^(lambda));
			end	
			
			return(probs)
		end


	### Simulate new probabilities
	
		function simulate_nested_probabilities_oos(b_est) 

			# preliminaries
			choices_oos = convert(Array,data_oos[!,:choice]);
			uninsured_plans_oos = convert(Array,data_oos[!,:uninsured_plan]);
			household_numbers_oos = convert(Array,data_oos[!,:household_number]);	
			unique_hh_oos = unique(household_numbers_oos);
			
			# set new x matrix
			X = form_X_matrix(all_covariates, data_oos, choices_oos, uninsured_plans_oos);
					
			# set coefficient estimats
			lambda = b_est[length(b_est)];
			beta = b_est[1:(length(b_est)-1)];
			U = X * beta;
			probs = zero(U);
			
			for id in unique_hh_oos
				household_ids = findall(household_numbers_oos .== id);
				exchange_household_ids = household_ids[1:(length(household_ids)-1)];
				uninsured_household_id = household_ids[length(household_ids)];
				
				probs[exchange_household_ids] = (exp.(U[exchange_household_ids]/lambda) * (sum(exp.(U[exchange_household_ids]/lambda)))^(lambda - 1)) ./ 
					(1 + (sum(exp.(U[exchange_household_ids]/lambda)))^(lambda));
				probs[uninsured_household_id] = 1 / (1 + (sum(exp.(U[exchange_household_ids]/lambda)))^(lambda));
			end	
			
			return(probs)
		end

		
	### Calculate Log Likelihood
	
		function calculate_log_likelihood(choices,probs)
			return((transpose(choices) * log.(probs))[1])
		end
	
	### Calculate Objective
	
		function newton_nested_logit(betap,X,choices,tolerance)
		
			# Initialization
			probs = simulate_nested_probabilities(betap,X);
			obj = calculate_log_likelihood(choices,probs);
			objprev = 0;
			println(obj)
			
			# Now iterate
			while abs(obj - objprev) > tolerance
			
				# Save old objective
				objprev = obj;
				
				# Calculate gradient and hessian
				grad = compute_nested_logit_gradient(betap,X,choices,probs);
				hess = compute_nested_logit_hessian(betap,X,choices,probs);
			
				# Update value of beta
				betap = betap - inv(hess) * grad;
				probs = simulate_nested_probabilities(betap,X);
				obj = calculate_log_likelihood(choices,probs); 
			
#				println(obj)
#				writedlm(filename,betap)
			end
			
			return(betap)
		end
	
		function objective_nested_logit(betap,X,choices) 
#			writedlm(filename,betap)
			probs = simulate_nested_probabilities(betap,X);
			return(-calculate_log_likelihood(choices,probs))
		end

	# Form Covariate Matrix
		
	function form_X_matrix(all_covariates, dataset, choicesize, unins)

		X = zeros(Float64,length(choicesize),length(all_covariates));	
		for k in all_covariates
			col = findall(all_covariates .== k)[1];
			if (k=="intercept")
				X[:,col] = 1 .- unins;
			else
				X[:,col] = convert(Array,dataset[!,k]);
			end
		end

		return(X)
	end	
		