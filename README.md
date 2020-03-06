# Decision Assitance and Health Insurance Choice

This repository provides the code files to replicate our paper, *Decision Assistance and Health Insurance Choice*. Below is the list of files and a brief description.

## Data
The data underlying this research were purchased from the State of California. Although we are not at liberty to share the data, we have made available all of our underlying code. Note that this is a living document, and additional scripts will be made available as necessary to facilitate a more complete replication of our results from the raw data.


## Files
1. [RunRender:](RunRender.R) This is an *R* script that calls the relevant packages/libraries, runs the analysis file, renders the paper and abstract, and renders any supporting online material.

2. [Analysis:](analysis/_Analysis.R) This is an *R* script that runs the main analysis. The file writes all tables and figures into seperate sub-directories (tables and figures, respectively). The file also saves a modified (smaller) version of the workspace that is subsequently used in rendering the TeX/PDF/HTML files. 

3. [Render:](_Abstract_ASHEcon_2019.Rmd) This is an *R Markdown* document that creates the TeX and PDF files from the analysis scripts. The document is fully reproducible in that all numbers, tables, figures, etc. are created directly from the analysis script and simply called within the markdown document. Note that the *knitr::render()* function in *R* strongly prefers everything in the render specification to be in the current directory. This is why the TeX/PDFs are saved in the active directory.
