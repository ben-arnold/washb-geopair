# washb-geopair

Geographic pair matching in large-scale cluster randomized trials


This repository includes R code to run all of the analysis for the paper:

Arnold et al. **Geographic pair matching in large-scale cluster randomized trials** (in review)

This work was funded by the National Institute of Allery and Infectious Diseases (R01-AI166671) and the original trials were funded by the Bill & Melinda Gates Foundation (OPPGD759).

Should you have any questions about the files in this repository, please contact Ben Arnold at UCSF (ben.arnold@ucsf.edu).

## Linked Repositories and Additional Resources

### Open Science Framework
This GitHub repository is mirrored on the Open Science Framework (OSF).  The OSF project page includes additional study-related resources, including the compiled HTML computational notebooks created from the `.Rmd` files, and the final analysis datasets that are created by `01-washb-geopair-data-processing.R`.

https://osf.io/cxb5e/


## _Nature_ Research Code Submission Items

Following: https://www.nature.com/documents/nr-software-policy.pdf

### System Requirements

All analyses were run using R software version 4.2.2 on Mac OSX Big Sur using the RStudio IDE (https://www.rstudio.com).

`> sessionInfo()`

`R version 4.3.0 (2023-04-21)`

`Platform: aarch64-apple-darwin20 (64-bit)`

`Running under: macOS Monterey 12.6`

In this repository we have created a Docker container and have used the `renv` package to archive the package versions so that you and reproduce the exact compute environment on an instance of R Studio Server, should you wish to do so. 

### Installation Guide and Instructions for Use (Desktop)

You can download and install R from CRAN: https://cran.r-project.org

You can download and install RStudio from their website: https://www.rstudio.com

All R packages required to run the analyses are sourced in the file `washb-geopair-Config.R`.

The installation time should be < 10 minutes total on a typical desktop computer.

To reproduce all analyses in the paper, we recommend that you: 

1. Clone the GitHub repository to your computer

For example, in the location on your computer where you would like to clone the repository, you could type into the Terminal command:

`git clone https://github.com/proctor-ucsf/trachoma-sero-transmission.git`

2. Recreate the exact package environment using the `renv` package. 

You can do this by opening the R project file ([washb-geopair.Rproj](https://github.com/ben-arnold/washb-geopair/blob/main/washb-geopair.Rproj)) in RStudio, loading the `renv` package, and typing `renv::restore()` to restore the package environment from the projects [renv.lock](https://github.com/ben-arnold/washb-geopair/blob/main/renv.lock) file. 

3. All of the analysis scripts should run smoothly (scripts `02-xx.Rmd` to `10-xx.Rmd`).  Note that scripts `11-13` rely on geographic coordinates of study clusters that constitute personally identifiable information. Therefore, those scripts will not run (GPS data not publicly available), but the scripts have been provided for transparency.

### Installation Guide and Instructions for Use (Docker / RStudio Server)

TBD

### Additional details

The first data processing script will download harmonized datasets from OSF and will create the final analysis datasets.

The data analyses on the above Mac desktop configuration required 9 minutes to run. 

Note that the only script that takes very long from those that will run on publicly available data (through script `10`) is `05-washb-geopair-estimate-icc.R` because bootstrapping mixed models using REML is computationally slow.

### License

This project is covered by the CC0 1.0 Universal license.
