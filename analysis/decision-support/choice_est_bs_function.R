# Meta --------------------------------------------------------------------

## Date Created:  5/12/2020
## Date Edited:   5/20/2020
## Description:   Function to estimate standard errors using bootstrap


# Estimation function -----------------------------------------------------

dchoice.bs <- function(t, r) {
  
  est.data <- get(paste0("est.data.",r,".",t))
  oos.data <- get(paste0("oos.data.",r,".",t))


  # oos.data <- oos.data %>%
  #   inner_join(est.data.bs %>% distinct(plan_name),
  #              by="plan_name") %>%
  #   group_by(household_number) %>%
  #   mutate(any_choice=max(choice)) %>%
  #   filter(any_choice==1) %>%
  #   ungroup() %>%
  #   select(-any_choice)
  
  ## Set up model and starting values
  all_covars <- as.list(bs.starting %>% filter(region==r, year==t, variable!="iv") %>% select(variable))$variable
  nested.formula <- formula(paste("choice ~", paste(all_covars, collapse=" + "),"| 0")) 
  beta_initial <- as.list(bs.starting %>% filter(region==r, year==t, variable!="iv") %>% select(estimate))$estimate
  
  ## Estimate model and store results
  test <- TRUE
  while (test==TRUE) {
    est.data.bs <- est.data %>%
      nest(-household_number)
        
    est.data.bs <- slice_sample(est.data.bs, n=nrow(est.data.bs), replace=TRUE) %>%
      mutate(household_number=cur_group_rows()) %>%
      unnest(data)

    nested.data <- mlogit.data(est.data.bs, choice="choice", shape="long", chid.var = "household_number", alt.var="plan_name")
    test <- is.error(mlogit(nested.formula, data=nested.data, weights=ipweight, 
                            nests=list(insured=nest.in, uninsured=nest.out), un.nest.el=TRUE, start=beta_initial))
    if (test==FALSE) {
      nested.logit <- mlogit(nested.formula, data=nested.data, weights=ipweight,
                             nests=list(insured=nest.in, uninsured=nest.out), un.nest.el=TRUE)
    } else {
      test <- TRUE
    }
  }
   
  coef.name <- as_tibble(names(nested.logit$coefficients)) %>%
    rename("variable"=value)
  coef.est <- as_tibble(nested.logit$coefficients) %>%
    rename("estimate"=value)
  coef.vals <- bind_cols(coef.name, coef.est) %>% 
    mutate(region=r)
      

  ## Out of sample predictions (predicted values for treated group)
  oos.nest <- mlogit.data(bind_rows(oos.data, est.data), choice="choice", shape="long", chid.var = "household_number", alt.var="plan_name")
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
      
  return(list("pred"=treated.dat, "coef"=coef.vals))        
      
}

