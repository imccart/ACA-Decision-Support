# Meta --------------------------------------------------------------------

## Date Created:  5/12/2020
## Date Edited:   5/20/2020
## Description:   Function to estimate standard errors using bootstrap


# Estimation function -----------------------------------------------------

dchoice.bs <- function(time, area) {
  
  for (t in time) {
    
    a.thresh <- as.numeric(afford.threshold %>% filter(year==t) %>% select(cutoff))
    
    step=0
    for (r in area) {
      step=step+1
    
      julia.data <- get(paste0("julia.data.",r,".",t))
      julia.oos <- get(paste0("julia.oos.",r,".",t))

      # Select bootstrap sample (within area and year)
      unique.hh <- julia.data %>%
        group_by(household_number) %>%
        mutate(hh_count=seq(n())) %>%
        filter(hh_count==1) %>%
        select(household_number) %>% ungroup()
      sample.hh <- sample_n(unique.hh, size=nrow(unique.hh), replace=TRUE)
      julia.data <- julia.data %>%
        inner_join(sample.hh, by=c("household_number"))
      
      
      # send sample to Julia and estimate with original starting values  
      julia_assign("data",julia.data)
      julia_assign("data_oos",julia.oos)
      
      all_covars <- as.list(bs.starting %>% filter(region==r, year==t, value!="lambda") %>% select(value))$value
      beta_initial <- as.list(bs.starting %>% filter(region==r, year==t) %>% select(value1))$value1
      
      jpath=getwd()
      unins.plans <- as.array(julia.data$uninsured_plan)
      julia_assign("uninsured_plans",unins.plans)
      julia_assign("jpath",jpath)
      julia_assign("all_covariates",all_covars)
      julia_assign("beta_initial",beta_initial)
      
      ## Estimate model in Julia and store results
      julia_source('analysis/J_demand_bs.jl')
      julia.out <- julia_eval('param_output')
      coef.name <- as_tibble(julia.out$Variable)
      coef.est <- as_tibble(julia.out$Coef)
      coef.vals <- bind_cols(coef.name, coef.est) %>% mutate(region=r)
      
      ## counterfactual estimates
      julia_assign("beta_hat",julia.out$Coef)
      pred.purchase <- julia_eval("simulate_nested_probabilities_oos(beta_hat)")
      pred.purchase <- as.data.frame(pred.purchase)
      colnames(pred.purchase) <- "pred_purchase"
      treated.dat <- bind_cols(julia.oos,pred.purchase)
      
      ## Finalize data for output
      julia_eval('GC.gc()')
      treated.dat <- treated.dat %>% mutate(region=r) %>%
        group_by(plan_name, region) %>% 
        summarize(tot_nonmiss=sum(!is.na(pred_purchase)),
                  obs_purchase=sum(choice, na.rm=TRUE),
                  pred_purchase=sum(pred_purchase, na.rm=TRUE),
                  tot_count=n())
      
      if (step==1) {
        final.data <- treated.dat
        final.coef <- coef.vals
      } else {
        final.data <- bind_rows(final.data, treated.dat)
        final.coef <- bind_rows(final.coef, coef.vals)
      }
      
      final.coef <- final.coef %>% mutate(year=t)
      final.data <- final.data %>% mutate(year=t)
      assign(paste0("final.data.",t),final.data)
      assign(paste0("final.coef.",t),final.coef)
      
      
    }    
  }
  
  step=0
  for (t in time) {
    step=step+1
    if (step==1) {
      all.predictions <- get(paste0("final.data.",t))
      all.coef <- get(paste0("final.coef.",t))
    } else {
      all.predictions <- bind_rows(all.predictions, get(paste0("final.data.",t)))
      all.coef <- bind_rows(all.coef, get(paste0("final.coef.",t)))
    }
  }
  
  return(list("pred"=all.predictions, "coef"=all.coef))
}  
  
