# Preliminaries -----------------------------------------------------------
afford.threshold <- tibble(
  cutoff = c(0.08, 0.0805, 0.0813, 0.0816, 0.0805, 0.0830),
  year = c(2014, 2015, 2016, 2017, 2018, 2019)
)

## specify time periods and regions for estimation
unique.region <- hh.clean %>% distinct(region)
time.dat <- 2014:2019
area.dat <- as.list(unique.region)$region

# Estimate model ----------------------------------------------------------
source('analysis/decision-support/choice_data_function.R')
source('analysis/decision-support/choice_est_function.R')
source('analysis/decision-support/choice_est_bs_function.R')


for (t in time.dat) {
  
  step=0
  for (r in area.dat) {
    step=step+1
    
    choice.data <- choice.data.fnc(t=t,r=r)
    assign(paste0("est.data.",r,".",t), choice.data$est.data)
    assign(paste0("oos.data.",r,".",t), choice.data$oos.data)
    nest.names <- unique(choice.data$est.data$plan_name)
    nest.in <- nest.names[nest.names != "Uninsured"]
    nest.out <- nest.names[nest.names == "Uninsured"]
    assign(paste0("nest.in.",r,".",t), nest.in)
    assign(paste0("nest.out.",r,".",t), nest.out)
    
    choice.est <- dchoice.est(d=choice.data$est.data, oos=choice.data$oos.data, t=t, r=r)
    
    treated.dat <- choice.est$pred
    coef.vals <- choice.est$coef
    if (step==1) {
      final.data <- treated.dat
      final.coef <- coef.vals
    } else {
      final.data <- bind_rows(final.data, treated.dat)
      final.coef <- bind_rows(final.coef, coef.vals)
    }
  }
  
  assign(paste0("final.data.",t),final.data)
  assign(paste0("final.coef.",t),final.coef)
  textme(msg=paste0(emo::ji("space"),"One small step for man...You've completed time: ",t))
  
}


step=0
for (t in time.dat) {
  step=step+1
  if (step==1) {
    est.prob <- get(paste0("final.data.",t))
    bs.starting <- get(paste0("final.coef.",t))
  } else {
    est.prob <- bind_rows(est.prob, get(paste0("final.data.",t)))
    bs.starting <- bind_rows(bs.starting, get(paste0("final.coef.",t)))
  }
}

all.coef <- bs.starting
all.prob <- est.prob


# Bootstrap Standard Errors -----------------------------------------------
max.boot <- 200

for (b in 1:max.boot) {

  t.samp <- sample_n(as.data.frame(time.dat), size=length(time.dat), replace=T)
  a.samp <- sample_n(as.data.frame(area.dat), size=length(area.dat), replace=T)
  
  tlist=as.list(t.samp)$time.dat
  rlist=as.list(a.samp)$area.dat
  for (t in tlist) {
    
    step=0
    for (r in rlist) {
      step=step+1
      
      nest.in <- get(paste0("nest.in.",r,".",t))
      nest.out <- get(paste0("nest.out.",r,".",t))
      
      bs.choice <- dchoice.bs(t=t, r=r)
      treated.dat <- bs.choice$pred
      coef.vals <- bs.choice$coef
      if (step==1) {
        final.data <- treated.dat
        final.coef <- coef.vals
      } else {
        final.data <- bind_rows(final.data, treated.dat)
        final.coef <- bind_rows(final.coef, coef.vals)
      }
    }
    
    assign(paste0("final.data.",t),final.data)
    assign(paste0("final.coef.",t),final.coef)
    
  }
  
  step=0
  for (t in tlist) {
    step=step+1
    if (step==1) {
      all.predictions <- get(paste0("final.data.",t))
      all.coef <- get(paste0("final.coef.",t))
    } else {
      all.predictions <- bind_rows(all.predictions, get(paste0("final.data.",t)))
      all.coef <- bind_rows(all.coef, get(paste0("final.coef.",t)))
    }
  }
  
  if (b==1) {
    bs.coef <- all.coef %>%
      mutate(boot=1)
    bs.pred <- all.predictions %>%
      mutate(boot=1)
  }
  if (b>1) {
    bs.coef <- bind_rows(bs.coef, all.coef %>% mutate(boot=b))
    bs.pred <- bind_rows(bs.pred, all.predictions %>% mutate(boot=b))  
  }
  
}

textme(msg=paste0(emo::ji("mage"),"  As it is written, so it has been done.  ",emo::ji("sparkles")))