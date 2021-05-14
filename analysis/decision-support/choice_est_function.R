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
  
  nested.data <- mlogit.data(d, choice="choice", shape="long", chid.var = "household_number", alt.var="plan_name")
  mclogit.formula <- formula(paste("cbind(choice, household_number) ~", paste(all_covars, collapse=" + ")))
  nested.formula <- formula(paste("choice ~", paste(all_covars, collapse=" + "),"| 0"))  
  logit.formula <- formula(paste("choice ~", paste(all_covars, collapse=" + ")))
  
  nest.names <- unique(d$plan_name)
  nest.in <- nest.names[nest.names != "Uninsured"]
  nest.out <- nest.names[nest.names == "Uninsured"]
  
  
  ## Find initial values from logit and apply to mclogit
  logit.start <- glm(logit.formula, data=d, family="binomial")
  test <- is.error(mclogit(mclogit.formula, data=d))
  if (test==FALSE) {
#    mc.logit <- mclogit(mclogit.formula, data=d)
    nested.logit <- mlogit(nested.formula, data=nested.data, nests=list(insured=nest.in, uninsured=nest.out), un.nest.el=TRUE)
  } else {
#    mc.logit <- mclogit(mclogit.formula, data=d, start=logit.start$coefficients[2:length(logit.start$coefficients)])
    nested.logit <- mlogit(nested.formula, data=nested.data, nests=list(insured=nest.in, uninsured=nest.out), un.nest.el=TRUE,
                           start=logit.start$coefficients[2:length(logit.start$coefficients)])    
  }


  ## MCLogit estimation
  coef.name <- as_tibble(names(nested.logit$coefficients)) %>%
    rename("variable"=value)
  coef.est <- as_tibble(nested.logit$coefficients) %>%
    rename("estimate"=value)
  coef.vals <- bind_cols(coef.name, coef.est) %>% 
    mutate(region=r,
           year=t)
  
  ## Out of sample predictions (predicted values for treated group)
  
  # treated.dat <- add_predictions(oos, mc.logit, var="pred_purchase", type="response") %>%
  #   mutate(region=r, year=t) %>%
  #   group_by(plan_name) %>%
  #   summarize(tot_nonmiss=sum(!is.na(pred_purchase)),
  #             obs_purchase=sum(choice, na.rm=TRUE),
  #             pred_purchase=sum(pred_purchase, na.rm=TRUE),
  #             tot_count=n())

  oos.nest <- mlogit.data(oos, choice="choice", shape="long", chid.var = "household_number", alt.var="plan_name")
  nested.pred <- predict(nested.logit, newdata=oos.nest)
  nested.pred <- as_tibble(nested.pred, rownames="household_number") %>% mutate(household_number=as.numeric(household_number))
  nested.pred <- nested.pred %>% pivot_longer(!household_number, names_to="plan_name", values_to="pred_purchase")
  treated.dat <- oos %>%
    left_join(nested.pred, by=c("household_number", "plan_name")) %>%
       mutate(region=r, year=t) %>%
       group_by(plan_name) %>% 
       summarize(tot_nonmiss=sum(!is.na(pred_purchase)),
                 obs_purchase=sum(choice, na.rm=TRUE),
                 pred_purchase=sum(pred_purchase, na.rm=TRUE),
                 tot_count=n())
    
  ## Finalize data for output
  return(list("pred"=treated.dat, "coef"=coef.vals))  
        
}
  