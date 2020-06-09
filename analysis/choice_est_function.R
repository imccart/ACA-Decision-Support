# Meta --------------------------------------------------------------------

## Date Created:  5/12/2020
## Date Edited:   5/25/2020
## Description:   Function to estimate demand, calls Julia within function and reports
##                mean predicted enrollments per plan as well as coefficient estimates


# Estimation function -----------------------------------------------------

dchoice.est <- function(time, area) {
  
  for (t in time) {
    
    a.thresh <- as.numeric(afford.threshold %>% filter(year==t) %>% select(cutoff))
    
    step=0
    for (r in area) {
      step=step+1
      
      julia.data <- get(paste0("julia.data.",r,".",t))
      julia.oos <- get(paste0("julia.oos.",r,".",t))
      
      julia_assign("data",julia.data)
      julia_assign("data_oos",julia.oos)
      
        
      ## Terms for estimation (passed to Julia)
      all_covars=c("intercept","premium", "AV", "HMO", "HSA", "silver")
      beta_initial = array(0,dim=length(all_covars)+1)
        
      ins.offer <- c(0,0,0,0)
      cml.share <- 0
      tot.choice <- sum(julia.data$choice==1)
      ins.offer[1] <- sum(julia.data$Anthem==1 & julia.data$choice==1)/tot.choice
      ins.offer[2] <- sum(julia.data$Blue_Shield==1 & julia.data$choice==1)/tot.choice
      ins.offer[3] <- sum(julia.data$Kaiser==1 & julia.data$choice==1)/tot.choice
      ins.offer[4] <- sum(julia.data$Health_Net==1 & julia.data$choice==1)/tot.choice
        
      cml.share <- ins.offer[1]
      if (ins.offer[1]>.1 & cml.share<.9) {
        all_covars=c(all_covars, "Anthem")
        beta_initial=c(beta_initial,0)
      }
      cml.share <- cml.share+ins.offer[2]
      if (ins.offer[2]>.1 & cml.share<.9) {
        all_covars=c(all_covars, "Blue_Shield")
        beta_initial=c(beta_initial,0)
      }
      cml.share <- cml.share+ins.offer[3]
      if (ins.offer[3]>.1 & cml.share<.9) {
        all_covars=c(all_covars, "Kaiser")
        beta_initial=c(beta_initial,0)
      }
      cml.share <- cml.share+ins.offer[4]
      if (ins.offer[4]>.1 & cml.share<.9) {
        all_covars=c(all_covars, "Health_Net")
        beta_initial=c(beta_initial,0)
      }
      
      jpath=getwd()
      unins.plans <- as.array(julia.data$uninsured_plan)
      julia_assign("uninsured_plans",unins.plans)
      julia_assign("jpath",jpath)
      julia_assign("all_covariates",all_covars)
      julia_assign("beta_initial",beta_initial)
      
      ## Estimate model in Julia and store results
      julia_source('analysis/J_demand_est.jl')
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
        
    }
    
    final.coef <- final.coef %>% mutate(year=t)
    final.data <- final.data %>% mutate(year=t)
    assign(paste0("final.data.",t),final.data)
    assign(paste0("final.coef.",t),final.coef)
    
    textme(msg=paste0(emo::ji("space"),"One small step for man...You've completed time: ",t))
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
  






#### Other Estimation Strategies ######
# # Using random sample of households
# unique.hh <- choices %>%
#   group_by(household_id) %>%
#   mutate(hh_count=seq(n())) %>%
#   filter(hh_count==1) %>%
#   select(household_id) %>% ungroup()
# 
# sample.hh <- sample_frac(unique.hh, size=0.5, replace=FALSE)
# sample.dat <- choices %>%
#   inner_join(sample.hh, by=c("household_id")) %>%
#   mutate(final_premium=final_premium/100) %>%
#   select(plan_choice, final_premium, household_id, Plan_Name) %>%
#   filter(!is.na(final_premium))


# ## Estimation using mlogit package    
# est.data <- mlogit.data(data.frame(untreated.dat), alt.var = "Plan_Name", chid.var = "household_id")
# oos.data <- mlogit.data(data.frame(treated.dat), alt.var = "Plan_Name", chid.var = "household_id")    

# ## Standard MNL
# mlogit1 <- mlogit(plan_choice ~ final_premium + any_0to17_prem + hh_size_prem +
#                      any_black_prem + any_hispanic_prem + FPL_250to400_prem + FPL_400plus_prem,
#                    data=est.data, reflevel="Uninsured")

## Nested logit
# nest.ins=as.list(unique(untreated.dat %>% select(Plan_Name) %>% filter(Plan_Name!="Uninsured")))[[1]]
# mlogit1 <- mlogit(plan_choice ~ final_premium + any_0to17_prem + hh_size_prem +
#                                 any_black_prem + any_hispanic_prem + FPL_250to400_prem + 
#                                 FPL_400plus_prem, 
#                   data=est.data,
#                   nests=list(aca=c(nest.ins), other='Uninsured'), un.nest.el=TRUE, reflevel='Uninsured')

# ## Estimate model with mixl package
# choice.list <- as.list(unique(untreated.dat %>% select(Plan_Name)))[[1]]
# mxlogit <- "\n "
# for (v in choice.list) {
#   step=step+1
#   v.spec <- paste0("U_",v,' = @B_premiume*$final_premium;\n ')
#   mxlogit <- paste0(mxlogit, v.spec)
# }
