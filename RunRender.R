
# Meta --------------------------------------------------------------------

## Title:         Decision Assistance and Health Insurance Choice
## Author:        Ian McCarthy
## Date Created:  10/28/2019
## Date Edited:   10/28/2019
## Description:   This file renders/runs all relevant R code for the project


# Preliminaries -----------------------------------------------------------

setwd('C:/Users/immccar/CloudStation/Professional/Research Projects/ACA Decision Support')
##setwd("D:/CloudStation/Professional/Research Projects/ACA Decision Support")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stargazer, knitr, kableExtra,
               lfe)

source('analysis/IanAnalysis.R')
rmarkdown::render(input = '_Abstract_ASHEcon_201910.Rmd',
                  output_format = 'all',
                  output_file ='_Abstract_ASHEcon_201910')
