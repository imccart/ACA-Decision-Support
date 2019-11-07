
# Meta --------------------------------------------------------------------

## Title:         Decision Assistance and Health Insurance Choice
## Author:        Ian McCarthy & Evan Saltzman
## Date Created:  10/28/2019
## Date Edited:   11/4/2019
## Description:   This file renders/runs all relevant R code for the project


# Preliminaries -----------------------------------------------------------

setwd('C:/Users/immccar/CloudStation/Professional/Research Projects/ACA Decision Support')
##setwd("D:/CloudStation/Professional/Research Projects/ACA Decision Support")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stargazer, knitr, kableExtra,
               lfe)


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
