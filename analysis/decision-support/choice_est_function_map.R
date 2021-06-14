# Meta --------------------------------------------------------------------

## Date Created:  5/12/2020
## Date Edited:   3/15/2021
## Description:   Function to estimate demand, reports mean predicted enrollments 
##                per plan as well as coefficient estimates


# Estimation function -----------------------------------------------------

dchoice.reg <- function(d, nest.names) {

  ## Terms for estimation
  all_covars=c("premium", "silver", "gold", "platinum")
  ins.offer <- c(0,0)
  cml.share <- 0
  tot.choice <- sum(d$choice==1)
  ins.offer[1] <- sum(d$Anthem==1 & d$choice==1)/tot.choice
  ins.offer[2] <- sum(d$Blue_Shield==1 & d$choice==1)/tot.choice
  ins.offer[3] <- sum(d$Kaiser==1 & d$choice==1)/tot.choice
  ins.offer[4] <- sum(d$Health_Net==1 & d$choice==1)/tot.choice

  all.hmo <- sum(d$HMO==1 & d$choice==1)/tot.choice
  if (all.hmo>.1) {
    all_covars=c(all_covars, "HMO")
  }
  all.hsa <- sum(d$HSA==1 & d$choice==1)/tot.choice
  if (all.hsa>.1) {
    all_covars=c(all_covars, "HSA")
  }
  
  cml.share <- ins.offer[1]
  if (ins.offer[1]>.4 & cml.share<.9) {
    all_covars=c(all_covars, "Anthem")
  }
  cml.share <- cml.share+ins.offer[2]
  if (ins.offer[2]>.4 & cml.share<.9) {
    all_covars=c(all_covars, "Blue_Shield")
  }
  cml.share <- cml.share+ins.offer[3]
  if (ins.offer[3]>.4 & cml.share<.9) {
    all_covars=c(all_covars, "Kaiser")
  }
  cml.share <- cml.share+ins.offer[4]
  if (ins.offer[4]>.4 & cml.share<.9) {
    all_covars=c(all_covars, "Health_Net")
  }  
  
  nested.data <- mlogit.data(d, choice="choice", shape="long", chid.var = "household_number", alt.var="plan_name")
  mclogit.formula <- formula(paste("cbind(choice, household_number) ~", paste(all_covars, collapse=" + ")))
  nested.formula <- formula(paste("choice ~", paste(all_covars, collapse=" + "),"| 0"))  
  logit.formula <- formula(paste("choice ~", paste(all_covars, collapse=" + ")))
  
  ## Find initial values from logit and apply to nested logit
  logit.start <- glm(logit.formula, data=d, family="binomial")
  test <- is.error(mlogit(nested.formula, data=nested.data, weights=ipweight, nests=list(insured=nest.names, uninsured="Uninsured"), un.nest.el=TRUE))
  if (test==FALSE) {
    nested.logit <- mlogit(nested.formula, data=nested.data, weights=ipweight, nests=list(insured=nest.names, uninsured="Uninsured"), un.nest.el=TRUE)
  } else {
    nested.logit <- mlogit(nested.formula, data=nested.data, weights=ipweight, nests=list(insured=nest.names, uninsured="Uninsured"), un.nest.el=TRUE,
                           start=logit.start$coefficients[2:length(logit.start$coefficients)])    
  }

  ## Finalize data for output
  return(nested.logit)
        
}