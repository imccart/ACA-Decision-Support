
# Meta --------------------------------------------------------------------

## Title:         Decision Assistance and Health Insurance Choice
## Author:        Ian McCarthy & Evan Saltzman
## Date Created:  10/28/2019
## Date Edited:   5/29/2020
## Description:   This file renders/runs all relevant R code for the project


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stargazer, knitr, kableExtra,
               lfe, modelr, mlogit, bookdown, future, parallel, nnet, mnlogit, mixl,
               JuliaCall, future.apply, gtsummary, emo, twilio)

source('paths.R')
source(paste0(common,"/common_functions.R"))
julia_setup(JULIA_HOME=jsetup)

# Load data ------------------------------------------------------------

plan.data <- read.csv("data/ca_plan_data2.csv") # Covered California plan data
data <- get(load("data/ca_enrollment_data_AUG022019")) # individual-level data
zip3.choices <- read.csv("data/zip3_choices2.csv",row.names = 1) # choice set by 3 digit zip and rating area
product.definitions <- read.csv("data/product_definitions.csv",row.names = 1) # definitions of column names in zip3_choices
households <- get(load("data/ca_household_data_AUG022019")) # household-level data
age.rating <- read.csv("data/age_rating_factors.csv",row.names=1)
pov.gdlines <- read.csv("data/poverty_guidelines.csv",row.names=1)
rating.areas <- read.csv("data/ca_rating_areas.csv",row.names=1)
contr.perc <- read.csv("data/contribution_percentages.csv",row.names=1)
outside_logit <- get(load("data/sipp_logit"))



# Clean data --------------------------------------------------------------
source('analysis/Data_Plans.R')
source('analysis/Data_Individual.R')
source('analysis/Data_Household.R')

final.data <- data.hh %>%
  mutate(channel = as.factor(channel),
         new_enrollee = is.na(previous_plan_number),
         any_assist = (channel=="Insurance Agent" | channel=="Other Assistance"),
         assist_agent = (channel=="Insurance Agent"),
         assist_other = (channel=="Other Assistance"),
         low_income = (FPL<1.5),
         hh_single = (household_size==1),
         hmo_ppo = (plan_network_type %in% c("HMO","PPO")),
         bad_obs = (new_enrollee==0 & year==2014),
         insurer = replace(insurer, 
                           insurer %in% c("Chinese_Community","LA_Care","Western","Contra_Costa","SHARP"),
                           "Other")) %>%
  filter( flagged==0 & !is.na(plan_number_nocsr) & bad_obs==0 & FPL<2.0)%>%
  select(lang_english, lang_spanish, lang_other,
         perc_0to17, perc_18to25, perc_26to34, perc_35to44, perc_45to54,
         perc_male, perc_asian, perc_black, perc_hispanic, perc_other, 
         FPL, low_income, household_size, hh_single, SEP, new_enrollee, 
         insurer, hmo_ppo, metal,
         channel, any_assist, assist_agent, assist_other, dominated_choice, 
         region, rating_area, year, household_id)



# Call analysis and set workspace for knitr -------------------------------
source('analysis/_ChoiceModel.R')
source('analysis/_SummaryStats.R')
source('analysis/_DominatedChoices.R')
source('analysis/_ChoiceSummary.R')


julia.list=NULL
for (i in 1:19) {
  for (t in 2014:2017) {
    julia.list <- c(julia.list, paste0("julia.data.",i,".",t), paste0("julia.oos.",i,".",t))
  }
}

rm(list=c(julia.list, "data", "data.clean", "data.hh", "data.ind", "hh.clean", "hh.language",
          "households", "max.age", "outside_logit", "plan.data", "sim.bs",
          "choice.data"))
save.image("data/R_workspace.Rdata")



# Run abstract markdown ---------------------------------------------------
load("data/R_workspace.Rdata")
rmarkdown::render(input = '_Abstract_ASHEcon_201910.Rmd',
                  output_format = 'all',
                  output_file ='_Abstract_ASHEcon_201910')

rmarkdown::render(input = '_Abstract_SHESG_202006.Rmd',
                  output_format = 'all',
                  output_file ='_Abstract_SHESG_202006')


# Run paper markdown ---------------------------------------------------
rmarkdown::render(input = 'Paper.Rmd',
                  output_format = 'all',
                  output_file ='Paper')

