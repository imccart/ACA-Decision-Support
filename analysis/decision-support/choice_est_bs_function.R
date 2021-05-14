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
    
      #choice.bs <- choice.data.fnc(t=t,r=r)
      #est.data <- choice.bs$est.data
      #oos.data <- choice.bs$oos.data
      est.data <- get(paste0("est.data.",r,".",t))
      oos.data <- get(paste0("oos.data.",r,".",t))

      ## Select bootstrap sample (within area and year)
      unique.hh <- est.data %>%
        group_by(household_number) %>%
        mutate(hh_count=seq(n())) %>%
        filter(hh_count==1) %>%
        select(household_number) %>% ungroup()
      sample.hh <- sample_n(unique.hh, size=nrow(unique.hh), replace=TRUE)
      est.data <- est.data %>%
        inner_join(sample.hh, by=c("household_number"))
      
      
      ## Set up model and starting values
      all_covars <- as.list(bs.starting %>% filter(region==r, year==t) %>% select(variable))$variable
      mclogit.formula <- formula(paste("cbind(choice, household_number) ~", paste(all_covars, collapse=" + ")))
      beta_initial <- as.list(bs.starting %>% filter(region==r, year==t) %>% select(estimate))$estimate

      nested.data <- mlogit.data(est.data, choice="choice", shape="long", chid.var = "household_number", alt.var="plan_name")
      nested.formula <- formula(paste("choice ~", paste(all_covars, collapse=" + "),"| 0"))  
    
      ## Estimate model and store results
      test <- is.error(mclogit(mclogit.formula, data=est.data, start=beta_initial))
      if (test==FALSE) {
        # mc.logit <- mclogit(mclogit.formula, data=est.data, start=beta_initial)  
        nested.logit <- mlogit(nested.formula, data=nested.data, nests=list(insured=nest.in, uninsured=nest.out), un.nest.el=TRUE,
                               start=beta_initial)        
      } else {
        # mc.logit <- mclogit(mclogit.formula, data=est.data)
        nested.logit <- mlogit(nested.formula, data=nested.data, nests=list(insured=nest.in, uninsured=nest.out), un.nest.el=TRUE)        
      }
      
      coef.name <- as_tibble(names(nested.logit$coefficients)) %>%
        rename("variable"=value)
      coef.est <- as_tibble(nested.logit$coefficients) %>%
        rename("estimate"=value)
      coef.vals <- bind_cols(coef.name, coef.est) %>% 
        mutate(region=r)
      

      ## Out of sample predictions (predicted values for treated group)
      # treated.dat <- add_predictions(oos.data, mc.logit, var="pred_purchase", type="response") %>%
      #   mutate(region=r) %>%
      #   group_by(plan_name) %>% 
      #   summarize(tot_nonmiss=sum(!is.na(pred_purchase)),
      #             obs_purchase=sum(choice, na.rm=TRUE),
      #             pred_purchase=sum(pred_purchase, na.rm=TRUE),
      #             tot_count=n())
      
      oos.nest <- mlogit.data(oos.data, choice="choice", shape="long", chid.var = "household_number", alt.var="plan_name")
      nested.pred <- predict(nested.logit, newdata=oos.nest)
      nested.pred <- as_tibble(nested.pred, rownames="household_number") %>% mutate(household_number=as.numeric(household_number))
      nested.pred <- nested.pred %>% pivot_longer(!household_number, names_to="plan_name", values_to="pred_purchase")
      treated.dat <- oos.data %>%
        left_join(nested.pred, by=c("household_number", "plan_name")) %>%
        mutate(region=r) %>%
        group_by(plan_name) %>% 
        summarize(tot_nonmiss=sum(!is.na(pred_purchase)),
                  obs_purchase=sum(choice, na.rm=TRUE),
                  pred_purchase=sum(pred_purchase, na.rm=TRUE),
                  tot_count=n())
      
      
      
      ## Finalize data for output
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
  
