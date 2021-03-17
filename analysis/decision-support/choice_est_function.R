# Meta --------------------------------------------------------------------

## Date Created:  5/12/2020
## Date Edited:   3/15/2021
## Description:   Function to estimate demand, reports mean predicted enrollments 
##                per plan as well as coefficient estimates


# Estimation function -----------------------------------------------------

dchoice.est <- function(d, oos, t, r) {
  a.thresh <- as.numeric(afford.threshold %>% filter(year==t) %>% select(cutoff))

  ## Terms for estimation
  all_covars=c("premium", "HMO", "HSA", "silver")
  ins.offer <- c(0,0,0,0)
  cml.share <- 0
  tot.choice <- sum(d$choice==1)
  ins.offer[1] <- sum(d$Anthem==1 & d$choice==1)/tot.choice
  ins.offer[2] <- sum(d$Blue_Shield==1 & d$choice==1)/tot.choice
  ins.offer[3] <- sum(d$Kaiser==1 & d$choice==1)/tot.choice
  ins.offer[4] <- sum(d$Health_Net==1 & d$choice==1)/tot.choice
  
  cml.share <- ins.offer[1]
  if (ins.offer[1]>.3 & cml.share<.9) {
    all_covars=c(all_covars, "Anthem")
  }
  cml.share <- cml.share+ins.offer[2]
  if (ins.offer[2]>.3 & cml.share<.9) {
    all_covars=c(all_covars, "Blue_Shield")
  }
  cml.share <- cml.share+ins.offer[3]
  if (ins.offer[3]>.3 & cml.share<.9) {
    all_covars=c(all_covars, "Kaiser")
  }
  cml.share <- cml.share+ins.offer[4]
  if (ins.offer[4]>.3 & cml.share<.9) {
    all_covars=c(all_covars, "Health_Net")
  }  
  
  mclogit.formula <- formula(paste("cbind(choice, household_number) ~", paste(all_covars, collapse=" + ")))
  logit.formula <- formula(paste("choice ~", paste(all_covars, collapse=" + ")))
  
  ## Find initial values from logit and apply to mclogit
  logit.start <- glm(logit.formula, data=d, family="binomial")
  test <- is.error(mclogit(mclogit.formula, data=d))
  if (test==FALSE) {
    mc.logit <- mclogit(mclogit.formula, data=d)
  } else {
    mc.logit <- mclogit(mclogit.formula, data=d, start=logit.start$coefficients[2:length(logit.start$coefficients)])
  }


  ## MCLogit estimation
  coef.name <- as_tibble(names(mc.logit$coefficients)) %>%
    rename("variable"=value)
  coef.est <- as_tibble(mc.logit$coefficients) %>%
    rename("estimate"=value)
  coef.vals <- bind_cols(coef.name, coef.est) %>% 
    mutate(region=r,
           year=t)
  
  ## Out of sample predictions (predicted values for treated group)
  treated.dat <- add_predictions(oos, mc.logit, var="pred_purchase", type="response") %>%
    mutate(region=r, year=t) %>%
    group_by(plan_name) %>% 
    summarize(tot_nonmiss=sum(!is.na(pred_purchase)),
              obs_purchase=sum(choice, na.rm=TRUE),
              pred_purchase=sum(pred_purchase, na.rm=TRUE),
              tot_count=n())
  
    
  ## Finalize data for output
  return(list("pred"=treated.dat, "coef"=coef.vals))  
        
}
  