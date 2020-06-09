# Decision Assitance and Insurer Steering in Health Insurance

This repository provides the code files to replicate our paper, *Decision Assistance and Insurer Steering in Health Insurance*. Below is the list of files and a brief description.

## Data
The data underlying this research were purchased from the State of California. Although we are not at liberty to share the data, we have made available all of our underlying code. Note that this is a living document, and additional scripts will be made available as necessary to facilitate a more complete replication of our results from the raw data.


## Files
1. **Compile**<br>
[RunRender](RunRender.R) is an *R* script that calls the relevant packages/libraries, cleans the data, runs the analysis files, renders the paper and abstract(s), and renders any supporting online material. All tables and figures are written into seperate sub-directories. The file also saves a modified (smaller) version of the workspace that is subsequently used in rendering the TeX/PDF/HTML files. 

2. **Data**<br>
We split the data management into three individual files.<br>
    - [Household Data](analysis/Data_Household.R) creates the household-level data objects
    - [Individual Data](analysis/Data_Individual.R) creates the individual-level data objects
    - [Plan Data](analysis/Data_Plans.R) creates the plan-level dataset

3. **Analysis**<br>
We split the analysis into three sections:<br>
    - [Choice Model](analysis/_ChoiceModel.R) runs the nested logit discrete choice model for estimating the causal effect of decision assistance on insurance choice. This file also calls several underlying scripts, including [choice_data_function](analysis/choice_data_function.R) to create the choice-level data, [choice_est_function](analysis/choice_est_function.R) to estimate the choice model, and [choice_est_bs_function](analysis/choice_est_bs_function.R) to bootstrap the standard errors/confidence intervals of the choice model. Note that the choice model is estimated in *Julia*, and the code uses the package *JuliaCall* to communicate between *RStudio* and *Julia*. Relevant functions for Julia are [J_demand_est](analysis/J_demand_est.jl) to estimate the main choice model, [J_demand_bs](analysis/J_demand_bs.jl) for boostrapping, and [J_demand_fnc](analysis/J_demand_fnc.jl) for other functions called by the former Julia files.
    - [Dominated Choices](analysis/_DominatedChoices.R) identifies the dominated choices and runs the reduced-form regression analysis of dominated choices as a function of decision assistance
    - Structural estimation (files pending)

4. **Summarize**<br>
We summarize our results with two individual code files:<br>
    - [Summary Statistics](analysis/_SummaryStats.R) calculates our main summary statistics
    - [Choice Summary](analysis/_ChoiceSummary.R) summarizes the nested logit results and forms the bootstrap confidence intervals


5. **Present**<br>
The main "deliverables" associated with the project, including all abstracts, presentations, the main paper, and any supplemental material. All documents are fully reproducible in that all numbers, tables, figures, etc. are created directly from the analysis script and simply called within the markdown document. Note that the *knitr::render()* function in *R* strongly prefers everything in the render specification to be in the current directory. This is why the TeX/PDFs are saved in the active directory.<br>
    - [ASHEcon Abstract, 2020](_Abstract_ASHEcon_201910.Rmd) This is an *R Markdown* document that creates the TeX and PDF files for our submission to the 2020 ASHEcon conference. The abstract was accepted for presentation, although ultimately not presented due to COVID-19.
    - [SHESG Abstract, 2020](_Abstract_SHESG_202006.Rmd) This is an *R Markdown* document that creates the TeX and PDF files for our submission to the 2020 SHESG conference. 
  
