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
plan(multisession, workers=4)
future.tracking <- c("time.dat","area.dat", "is.error")

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
    future.tracking <- c(future.tracking, paste0("est.data.",r,".",t), paste0("oos.data.",r,".",t), "nest.in", "nest.out")      
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

future.tracking <- c(future.tracking, "bs.starting")


# Bootstrap Standard Errors -----------------------------------------------
bootsrp <- function(j) {
  t.samp <- sample_n(as.data.frame(time.dat), size=length(time.dat), replace=T)
  a.samp <- sample_n(as.data.frame(area.dat), size=length(area.dat), replace=T)
  bs.choice <- dchoice.bs(time=as.list(t.samp)$time.dat, area=as.list(a.samp)$area.dat)
  return(list("coef"=bs.choice$coef, "pred"=bs.choice$pred))
}
future.tracking <- c(future.tracking, "dchoice.bs", "afford.threshold")


max.boot <- 200
 sim.bs <- future_lapply(1:max.boot, 
                         bootsrp, 
                         future.globals=future.tracking, 
                         future.packages=c("dplyr","mclogit","modelr"),
                         future.seed=TRUE)


 bs.coef <- sim.bs[[1]]$coef %>%
  mutate(boot=1)
bs.pred <- sim.bs[[1]]$pred %>%
  mutate(boot=1)
for (i in 2:max.boot) {
  bs.coef <- bind_rows(bs.coef, sim.bs[[i]]$coef %>% mutate(boot=i))
  bs.pred <- bind_rows(bs.pred, sim.bs[[i]]$pred %>% mutate(boot=i))  
}


textme(msg=paste0(emo::ji("mage"),"  As it is written, so it has been done.  ",emo::ji("sparkles")))