# Decision Assitance and Insurer Steering in Health Insurance

This repository provides the code files to replicate our paper, *Decision Assistance and Insurer Steering in Health Insurance*. Below is the list of files and brief descriptions.

## Raw Data {#raw}
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


In addition,
10. Public Use Microdata Areas: Data downloaded directly from Census Bureau and saved as *PUMAs.csv*
11. Small Area Health Insurance Estimates: Data for 2014 downloaded from Census Bureau and saved as *sahie_2014.csv*
12. *counties_2014* through *counties_2019*
13. Rate filing: Raw data downloaded from [CMS website](https://www.cms.gov/CCIIO/Resources/Data-Resources/ratereview) and merged into *2014-2020.RData* file. 
14. Medical loss ratio: Raw data downloaded from [CMS website](https://www.cms.gov/CCIIO/Resources/Data-Resources/mlr). Data are stored in separate folders, "2014 MLR Data", "2015 MLR Data", etc.
15. RA/RI: PDFs downloaded from CMS website. Manually extracted relevant information into *ra_reins.csv* file.


## Data Management and Cleaning
The following code files build the final analytic datasets used for both our demand and supply-side analysis. All of the raw data files are listed above, and any data files created from the following code files are saved into a `/data/final/` subdirectory.

1. [Process SIPP](data-code/process.SIPP.R): This process some data based on the SIPP. It creates two final datasets, an *acs_immigration.csv* dataset and an *acs_emp_offer.csv* dataset. These files are inputs into the [final data code](data-code/process.final.data.R).
2. [Impute SIPP](data-code/impute.SIPP.R): This generates a prediction model for the outside option, *sipp-logit*.
3. [Process COVCAL data](data-code/process.COVCAL.data.R): This is the primary code file to import and clean the data. This code takes as inputs the [raw data](#raw) objects [1]-[7], and creates two primary data objects: 1) a dataset on enrollments and plan information at the individual level, *enroll_temp.Rdata*; and 2) a dataset on enrollments and plan information at the household level, *household_temp.Rdata*.
4. [Process final data](data-code/process.final.data.R): This file incorporates ACS data (item [8] in the [raw data](#raw)), along with data on PUMAs, SAHIE, and California county information into the temporary enrollment and household objects from the prior step. This file ultimately creates three final data objects: 1) an enrollment dataset, *enroll_data.Rdata*; 2) a household-level dataset, *household_data.Rdata*; and 3) a plan-level dataset, *plan_data.Rdata*.

[Build Final Data](data-code/build-data.R) is an *R* script that sets initial paths, installs necessary packages, and calls all of the above code files. 




## Analysis

We split the analysis into three sections. First, we


    - [Choice Model](analysis/_ChoiceModel.R) runs the nested logit discrete choice model for estimating the causal effect of decision assistance on insurance choice. This file also calls several underlying scripts, including [choice_data_function](analysis/choice_data_function.R) to create the choice-level data, [choice_est_function](analysis/choice_est_function.R) to estimate the choice model, and [choice_est_bs_function](analysis/choice_est_bs_function.R) to bootstrap the standard errors/confidence intervals of the choice model. Note that the choice model is estimated in *Julia*, and the code uses the package *JuliaCall* to communicate between *RStudio* and *Julia*. Relevant functions for Julia are [J_demand_est](analysis/J_demand_est.jl) to estimate the main choice model, [J_demand_bs](analysis/J_demand_bs.jl) for boostrapping, and [J_demand_fnc](analysis/J_demand_fnc.jl) for other functions called by the former Julia files.
    - [Dominated Choices](analysis/_DominatedChoices.R) identifies the dominated choices and runs the reduced-form regression analysis of dominated choices as a function of decision assistance
    - Structural estimation (files pending)




2. [Process MLR data](data-code/process.MLR.data.nav.R): 


## Papers

calls the relevant packages/libraries, cleans the data, runs the analysis files, renders the paper and abstract(s), and renders any supporting online material. All tables and figures are written into separate sub-directories. The file also saves a modified (smaller) version of the workspace that is subsequently used in rendering the TeX/PDF/HTML files. 


## Presentations


4. **Summarize**<br>
We summarize our results with two individual code files:<br>
    - [Summary Statistics](analysis/_SummaryStats.R) calculates our main summary statistics
    - [Choice Summary](analysis/_ChoiceSummary.R) summarizes the nested logit results and forms the bootstrap confidence intervals


5. **Present**<br>
The main "deliverables" associated with the project, including all abstracts, presentations, the main paper, and any supplemental material. All documents are fully reproducible in that all numbers, tables, figures, etc. are created directly from the analysis script and simply called within the markdown document. Note that the *knitr::render()* function in *R* strongly prefers everything in the render specification to be in the current directory. This is why the TeX/PDFs are saved in the active directory.<br>
    - [ASHEcon Abstract, 2020](_Abstract_ASHEcon_201910.Rmd) This is an *R Markdown* document that creates the TeX and PDF files for our submission to the 2020 ASHEcon conference. The abstract was accepted for presentation, although ultimately not presented due to COVID-19.
    - [SHESG Abstract, 2020](_Abstract_SHESG_202006.Rmd) This is an *R Markdown* document that creates the TeX and PDF files for our submission to the 2020 SHESG conference. 
  
