# Meta --------------------------------------------------------------------

## Date Created:  3/4/2020
## Date Edited:   5/12/2020
## Description:   Create data for discrete choice model and estimate by year and 3-digit zip

## Notes:         This file requires that we've already created the household data
##                and the plan data.



# Clean data --------------------------------------------------------------

## Remove "flagged" households
hh.clean <- data.hh %>% filter(flagged==0)
data.clean <- data %>% filter(flagged==0)

## Oldest person per household
max.age <- data.ind %>%
  group_by(household_id, year) %>%
  summarize(oldest_member=max(AGE))

## Add some variables
hh.clean <- hh.clean %>%
  mutate(FPL_bracket = case_when(
    FPL <= 1.38 ~ "138orless",
    FPL > 1.38 & FPL <= 2.50 ~ "138to250",
    FPL > 2.50 & FPL <= 4 ~ "250to400",
    FPL > 4 ~ "400ormore")) %>%
  mutate(perc_18to34 = perc_18to25 + perc_26to34,
         perc_35to54 = perc_35to44 + perc_45to54,
         FPL_bracket=as.factor(FPL_bracket)) %>%
  left_join(max.age, by=c("household_id","year"))

  

## Predict who left market
hh.clean <- hh.clean %>%
  add_predictions(outside_logit, "pred_oom", type="response") %>%
  mutate(pred_oom = ifelse(is.na(plan_number_nocsr),pred_oom,NA),
         unif_draw=runif(n(),0,1),
         out_of_market = (pred_oom >= unif_draw)) %>%
  filter(out_of_market == FALSE | !is.na(plan_number_nocsr))
hh.clean <- as_tibble(hh.clean)
  

data.clean <- data.clean %>%
  left_join(hh.clean %>% select(household_id, year, out_of_market), 
            by=c("household_id","year")) %>%
  filter(out_of_market == FALSE | !is.na(plan_number_nocsr))
data.clean <- as_tibble(data.clean)


# Form choice sets --------------------------------------------------------
afford.threshold <- tibble(
  cutoff = c(0.08, 0.0805, 0.0813, 0.0816, 0.0805, 0.0830),
  year = c(2014, 2015, 2016, 2017, 2018, 2019)
)

zip3.choices <- zip3.choices %>%
  mutate(outside_option=1)
zip3 <- as_tibble(zip3.choices %>% distinct(zip3))
unique.region <- as_tibble(zip3.choices %>% distinct(Region))


## specify time periods and regions for estimation
plan(multiprocess, workers=4)

time.dat <- 2014:2017
area.dat <- as.list(unique.region)$Region


## form datasets
future.tracking <- c("time.dat","area.dat")

source('analysis/choice_data_function.R')
for (t in time.dat) {
  for (r in area.dat) {
    choice.data <- choice.data.fnc(t=t,r=r)
    assign(paste0("julia.data.",r,".",t),choice.data$julia.data)
    assign(paste0("julia.oos.",r,".",t),choice.data$julia.oos)
    future.tracking <- c(future.tracking, paste0("julia.data.",r,".",t), paste0("julia.oos.",r,".",t))
  }
}


# Estimate full model -----------------------------------------------------

source('analysis/choice_est_function.R')
dyn.load(paste0(jsetup,"/libopenlibm.DLL"))
julia_source('analysis/J_demand_fnc.jl')

choice.est <- dchoice.est(time=time.dat, area=area.dat)
est.prob <- choice.est$pred
bs.starting <- choice.est$coef

future.tracking <- c(future.tracking, "bs.starting")

# Bootstrap Standard Errors -----------------------------------------------
source('analysis/choice_est_bs_function.R')
bootsrp <- function(j) {
  julia_setup(JULIA_HOME=jsetup)  
  julia_source('analysis/J_demand_fnc.jl')
  t.samp <- sample_n(as.data.frame(time.dat), size=length(time.dat), replace=T)
  a.samp <- sample_n(as.data.frame(area.dat), size=length(area.dat), replace=T)
  bs.choice <- dchoice.bs(time=as.list(t.samp)$time.dat, area=as.list(a.samp)$area.dat)
  return(list("coef"=bs.choice$coef, "pred"=bs.choice$pred))
}
future.tracking <- c(future.tracking, "dchoice.bs", "afford.threshold", "jsetup")


max.boot <- 200
sim.bs <- future_lapply(1:max.boot, 
                        bootsrp, 
                        future.globals=future.tracking, 
                        future.packages=c("JuliaCall","dplyr"))
bs.coef <- sim.bs[[1]]$coef
bs.pred <- sim.bs[[1]]$pred
for (i in 2:max.boot) {
  bs.coef <- bind_rows(bs.coef, sim.bs[[i]]$coef)
  bs.pred <- bind_rows(bs.pred, sim.bs[[i]]$pred)  
}


textme(msg=paste0(emo::ji("mage"),"  As it is written, so it has been done.  ",emo::ji("sparkles")))