
# Meta --------------------------------------------------------------------

## Title:         Decision Assistance and Health Insurance Choice
## Author:        Ian McCarthy & Evan Saltzman
## Date Created:  10/28/2019
## Date Edited:   6/1/2021
## Description:   This file calls all scripts necessary for estimating the effect of decision assistance on 
##                health insurance choice.


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) renv::install('pacman')
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stargazer, knitr, kableExtra,
               lfe, modelr, mclogit, bookdown, parallel, nnet, mixl, fixest,
               future.apply, gtsummary, emo, twilio, SAScii, data.table, scales, mlogit,
               modelsummary, cobalt, WeightIt, broom, tidymodels, webshot)

source('paths.R')
source(paste0(common,"/common_functions.R"))
set.seed(84324) # set using random.seed(s=Sys.time()) 



# Import and clean data ---------------------------------------------------

plan_data <- get(load("data/final/plan_data"))
data <- get(load("data/final/enroll_data"))
households <- get(load("data/final/household_data"))
outside_logit <- get(load("data/final/sipp_logit"))

plan_data <- as_tibble(plan_data) %>%
  mutate(metal = as.character(Metal_Level),
         metal = replace(metal, metal %in% c("Silver - Enhanced 73",
                                             "Silver - Enhanced 87",
                                             "Silver - Enhanced 94"), 
                         "Silver") )


## Oldest person per household
data <- as.data.table(data, key=c("household_id","year"))
max.age <- data[, .(oldest_member = max(AGE, na.rm=T)), by=.(household_id,year)]

## language spoken
language <- data[, .(household_id, year, lang_english=(language_spoken=="English"), 
                     lang_spanish=(language_spoken=="Spanish"),
                     lang_other=(! language_spoken %in% c("English","Spanish")))]

hh.language <- language[, .(english=max(lang_english, na.rm=T),
                            spanish=max(lang_spanish, na.rm=T),
                            other_language=max(lang_other)), by=.(household_id,year)]
rm("data")

## Remove "flagged" households
hh.full <- as_tibble(households) %>%
  filter(flagged==0)

## Add FPL and max age
hh.full <- hh.full %>%
  mutate(FPL_bracket = case_when(
    FPL <= 1.38 ~ "138orless",
    FPL > 1.38 & FPL <= 2.50 ~ "138to250",
    FPL > 2.50 & FPL <= 4 ~ "250to400",
    FPL > 4 ~ "400ormore")) %>%
  mutate(perc_18to34 = perc_18to25 + perc_26to34,
         perc_35to54 = perc_35to44 + perc_45to54,
         FPL_bracket=as.factor(FPL_bracket)) %>%
  left_join(max.age, by=c("household_id","year")) %>%
  left_join(hh.language, by=c("household_id","year"))


## Predict who left market and focus only on those in the market
hh.full <- hh.full %>%
  add_predictions(outside_logit, "pred_oom", type="response") %>%
  mutate(pred_oom = ifelse(is.na(plan_number_nocsr),pred_oom,NA),
         unif_draw=runif(n(),0,1),
         out_of_market = (pred_oom >= unif_draw)) %>%
  filter(out_of_market == FALSE | !is.na(plan_number_nocsr))


## collect/create relevant plan characteristic variables needed for choice model
hh.full <- hh.full %>%
  separate(zip_region_year, c(NA,"region",NA), sep="_") %>%
  mutate(region = as.integer(region)) %>%
  left_join(plan_data %>% 
              select(plan_unique_id,
                     plan_network_type=PLAN_NETWORK_TYPE,
                     metal, 
                     insurer=Insurer),
            by=c("plan_unique_id")) %>%
  mutate(metal = replace(metal, metal == "Minimum Coverage", "Catastrophic"),
         subsidy_eligible = as.numeric(subsidized_members>0),
         subsidized = case_when(
           subsidy_eligible == 1 ~ "Subsidized",
           subsidy_eligible == 0 ~ "Unsubsidized",
           TRUE ~ NA_character_
         ),
         csr_eligible = as.numeric(subsidized == "Subsidized" & FPL <= 2.50)
  ) 

hh.full <- hh.full %>%
  mutate(dominated_choice = 
           case_when(
             csr_eligible == 1 & 
               subsidy_linear_piece %in% c("138% FPL or less","138% FPL to 150% FPL") &
               metal %in% c("Gold","Platinum") ~ 1,
             csr_eligible == 1 & 
               subsidy_linear_piece %in% c("150% FPL to 200% FPL") &
               metal == "Gold" ~ 1,
             csr_eligible == 1 &
               subsidy_linear_piece %in% c("138% FPL or less","138% FPL to 150% FPL") &
               !(metal %in% c("Gold","Platinum")) ~ 0,
             csr_eligible == 1 & 
               subsidy_linear_piece %in% c("150% FPL to 200% FPL") &
               metal != "Gold" ~ 0,
             TRUE ~ NA_real_
           )) %>%
  mutate(channel=ifelse(navigator==1 | broker==1 | agent==1, "Assisted", "Unassisted"),
         channel_detail=case_when(
           navigator==1 ~ "Navigator",
           broker==1 | agent==1 ~ "Agent",
           TRUE ~ "Unassisted"
         ),
         assisted=ifelse(channel=="Assisted",1,0),
         channel_detail=as.factor(channel_detail),
         any_agent=ifelse(broker==1 | agent==1, 1, 0),
         new_enrollee=ifelse(is.na(previous_plan_offered),1,0),
         switch=case_when(
           new_enrollee==0 & !is.na(plan_number_nocsr) & !is.na(previous_plan_number) & plan_number_nocsr!=previous_plan_number ~ 1,
           new_enrollee==0 & !is.na(plan_number_nocsr) & !is.na(previous_plan_number) & plan_number_nocsr==previous_plan_number ~ 0,
           TRUE ~ NA_real_
         ))


## Filter for only new purchases for plan choice analysis
hh.clean <- hh.full %>% 
  filter(is.na(previous_plan_offered))

hh.ins <- hh.full %>% filter(!is.na(plan_number_nocsr))  

# Run analysis scripts ----------------------------------------------------

source('analysis/decision-support/_SummaryStats.R')
#source('analysis/decision-support/_DominatedChoices.R')

rm("hh.full","households","max.age","language","hh.language","panel.length",
   "hh.nest","mod.full", "sim.bs")
source('analysis/decision-support/_ChoiceModel.R')
source('analysis/decision-support/_ChoiceSummary.R')

