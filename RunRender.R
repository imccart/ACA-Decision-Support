
# Meta --------------------------------------------------------------------

## Title:         Decision Assistance and Health Insurance Choice
## Author:        Ian McCarthy & Evan Saltzman
## Date Created:  10/28/2019
## Date Edited:   3/4/2020
## Description:   This file renders/runs all relevant R code for the project


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stargazer, knitr, kableExtra,
               lfe, modelr)



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


# Call analysis and set workspace for knitr -------------------------------
source('analysis/_Analysis.R')
rm(list=c("data", "data.ind", "households", "data.hh", "zip3.choices",
          "plan.data", "product.definitions", "final.data", "hh.language"))
save.image("data/R_workspace.Rdata")



# Run abstract markdown ---------------------------------------------------
rmarkdown::render(input = '_Abstract_ASHEcon_201910.Rmd',
                  output_format = 'all',
                  output_file ='_Abstract_ASHEcon_201910')
