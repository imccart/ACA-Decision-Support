
# Meta --------------------------------------------------------------------

## Title:         Decision Assistance and Health Insurance Choice
## Author:        Ian McCarthy & Evan Saltzman
## Date Created:  10/28/2019
## Date Edited:   3/2/2021
## Description:   This file builds the final datasets for analysis


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) renv::install('pacman')
pacman::p_load(tidyverse, dplyr, lubridate, SAScii, data.table)
source('paths.R')

# Import and clean data ---------------------------------------------------
source('data-code/process.SIPP.R')
rm(list=ls())
source('data-code/impute.SIPP.R')
source('data-code/process.COVCAL.data.R')


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

