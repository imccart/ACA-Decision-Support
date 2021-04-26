# Decision Assitance and Insurer Steering in Health Insurance

This repository provides the code files to replicate our paper, *Decision Assistance and Insurer Steering in Health Insurance*. Below is the list of files and brief descriptions.

## Raw Data
The primary data underlying this research were purchased from the State of California. Although we are not at liberty to share the data, we have made available all of our underlying code. Note that this is a living document, and additional scripts will be made available as necessary to facilitate a more complete replication of our results from the raw data. The raw data files are listed below, along with a brief description. With regard to file and folder structure, each of these files are stored in the subdirectory, `/data/`.

The following is a list of the core raw data files used throughout our analysis:

1. Enrollments: Data obtained via FOIA directly from Covered California (*pra_07192019.csv*).
2. Plans: These data were downloaded directly from the Covered California [website](downloaded from Covered California website https://hbex.coveredca.com/data-research/) and manually appended in Excel. The appended file is *plan_data.csv*, with descriptions of each plan (the column headings in *plan_data.csv*) detailed in *product_definitions.csv*.
3. Choices: Raw data for 2017-2019 were downloaded directly, while data for 2014-2016 were obtained through FOIA. These files were manually appended to create the *zip3_choices.csv* file.
4. Age rating: Consumer Information and Insurance Oversite (CCIIO) age rating curves from CMS, *age_rating_factors.csv*
5. Poverty thresholds: *poverty_guidelines.csv*
6. ACA contribution percentages: *contribution_percentages.csv*
7. California rating areas: *rating_areas.csv*
8. ACS and SIPP: Several datasets used to collect and compile information from the ACS and SIPP. All data files are located in the `/data/ACS-SIPP/` subfolder.



In addition, we use the following **supplemental datasets** in forming our final individual, household, and plan-level files:

9. Public Use Microdata Areas: Data downloaded directly from Census Bureau and saved as *PUMAs.csv*
10. Small Area Health Insurance Estimates: Data for 2014 downloaded from Census Bureau and saved as *sahie_2014.csv*
11. *counties_2014* through *counties_2019*


Finally, our **steering analysis** further employs the following datasets:

12. Rate filing: Raw data downloaded from [CMS website](https://www.cms.gov/CCIIO/Resources/Data-Resources/ratereview) and merged into *2014-2020.RData* file. 
13. Medical loss ratio: Raw data downloaded from [CMS website](https://www.cms.gov/CCIIO/Resources/Data-Resources/mlr). Data are stored in separate folders, "2014 MLR Data", "2015 MLR Data", etc.
14. RA/RI: PDFs downloaded from CMS website. Manually extracted relevant information into *ra_reins.csv* file.


## Data Management and Cleaning
The following code files build the final analytic datasets used for our analysis. All of the raw data files are listed [above](#raw-data), and any data files created from the following code files are saved into a `/data/final/` subdirectory. In general, we split the data management and organization into two: 1) data for an analysis of the effects of decision assistance; and 2) data for a structural analysis of insurer steering. All of the relevant scripts are listed and briefly described in the below subsections. 

### Decision assistance data
[Build Demand Data](data-code/build-demand-data.R) is an *R* script that sets initial paths, installs necessary packages, and calls all of the relevant code files. This script calls the following individual files:

1. [Process SIPP](data-code/process.SIPP.R): This processes the SIPP data. It creates two final datasets, an *acs_immigration.csv* dataset and an *acs_emp_offer.csv* dataset. These files are inputs into the [final data code](data-code/process.final.data.R).
2. [Impute SIPP](data-code/impute.SIPP.R): This generates a prediction model for the outside option, *sipp-logit*.
3. [Process COVCAL data](data-code/process.COVCAL.data.R): This is the primary code file to import and clean the data. This code takes as inputs the [raw data](#raw-data) objects [1]-[7], and creates two primary data objects: 1) a dataset on enrollments and plan information at the individual level, *enroll_temp.Rdata*; and 2) a dataset on enrollments and plan information at the household level, *household_temp.Rdata*.
4. [Prepare demand data](data-code/prepare.demand.data.R): This file incorporates ACS data (item [8] in the [raw data](#raw-data)), along with data on PUMAs, SAHIE, and California county information into the temporary enrollment and household objects from the prior step. This file ultimately creates three final data objects: 1) an enrollment dataset, *enroll_data.Rdata*; 2) a household-level dataset, *household_data.Rdata*; and 3) a plan-level dataset, *plan_data.Rdata*.


### Steering data
1. [Process MLR data](data-code/process.MLR.data.nav.R): This code organizes the loss ratio data and incorporates CMS reports for RA and RI. It creates one final dataset, *mlr_data.csv*
2. [Process rate data](data-code/process.rate.data.R): This collects the rate filing data 
3. [Demand estimation](analysis/demand.est.R): This runs a preliminary demand estimation for input into the steering analysis. This analysis is based on the output of the [prepare demand data](data-code/prepare.demand.data.R) script discussed above.
4. [Prepare supply data](data-code/prepare.supply.data.R): This file incorporates the output from the rate data and MLR data, as well as the choice probabilities from the demand estimation, to form the data objects necessary for the supply-side analysis.



## Analysis

We split the analysis into two overarching sections: 1) a standalone analysis focusing on estimating the causal effect of decision support on plan choice; and 2) a standalone structural analysis of insurer steering, which incorporates both the decision assistance data objects and the steering data objects. We briefly discuss the relevant code files for each analysis below.


### Effect of decision support
Our initial analysis of decision support proceeds in three steps:

1. **Summarize**<br>
We summarize our results with two individual code files:<br>
    - [Summary Statistics](analysis/_SummaryStats.R) calculates our main summary statistics
    - [Choice Summary](analysis/_ChoiceSummary.R) summarizes the nested logit results and forms the bootstrap confidence intervals

2. **Dominated choices**<br>
We consider reduced-form evidence for the role of decision support in the [Dominated Choices](analysis/_DominatedChoices.R) script. This code file identifies the dominated choices and runs an initial regression analysis of dominated choices as a function of decision assistance

3. **Choice model**<br>
[Choice Model](analysis/_ChoiceModel.R) runs the nested logit discrete choice model for estimating the causal effect of decision assistance on insurance choice. This file also calls several underlying scripts, including [choice_data_function](analysis/choice_data_function.R) to create the choice-level data, [choice_est_function](analysis/choice_est_function.R) to estimate the choice model, and [choice_est_bs_function](analysis/choice_est_bs_function.R) to bootstrap the standard errors/confidence intervals of the choice model.

### Structural analysis of steering
  - Structural estimation (files pending)
  - extra




## Papers and presentations
Below are links to different abstracts, presentations, and the main paper. 

- [Emory Brown Bag Abstract, 2021](finals/abstract-emory-lunchlearn-202103.tex) This is a *Tex* file to generate the abstract for an internal presentation as part of our departmental brown bag series. 
    
- [Emory Brown Bag Presentation, 2021](finals/lunch-and-learn/lunch-learn-202103.html) This is a brown bag presentation from March 2021.

 


  
