# washb-geopair

## Geographic pair matching in large-scale cluster randomized trials


This repository includes R code to run all of the analysis for the paper:

Arnold BF, Rerolle F, Tedijanto C et al. **Geographic pair matching in large-scale cluster randomized trials**. _Nature Communications_ 15, 1069 (2024). https://doi.org/10.1038/s41467-024-45152-y
This work was funded by the National Institute of Allery and Infectious Diseases (R01-AI166671) and the Bill & Melinda Gates Foundation (OPPGD759).

If you have any questions about the files in this repository, please contact Ben Arnold at UCSF (ben.arnold@ucsf.edu).

## Linked Repositories and Additional Resources

### Open Science Framework
This GitHub repository is mirrored on the Open Science Framework (OSF).  The OSF project page includes additional study-related resources, including the compiled HTML computational notebooks created from the `.Rmd` files, and the final analysis datasets that are created by [`00-washb-geopair-data-processing.R`](https://github.com/ben-arnold/washb-geopair/blob/main/R/00-washb-geopair-data-processing.R).  You can download the public datasets to your local repository using the script [`01-washb-geopair-download-public-data.R`](https://github.com/ben-arnold/washb-geopair/blob/main/R/01-washb-geopair-download-public-data.R)

https://osf.io/cxb5e/

## _Nature_ Research Code Submission Items

Following: https://www.nature.com/documents/nr-software-policy.pdf

### System Requirements

All analyses were run using R software version 4.3.2 on Mac OSX Monterey using the RStudio IDE (https://www.rstudio.com).

`> sessionInfo()`

`R version 4.3.2 (2023-10-31)`

`Platform: aarch64-apple-darwin20 (64-bit)`

`Running under: macOS Monterey 12.6`

In this repository we have used the `renv` package to archive the package versions so that you and reproduce the exact compute environment, should you wish to do so. 

### Installation Guide and Instructions for Use (Desktop)

You can download and install R from CRAN: https://cran.r-project.org

You can download and install RStudio from their website: https://www.rstudio.com

All R packages required to run the analyses are sourced in the file `washb-geopair-Config.R`.

The installation time should be < 10 minutes total on a typical desktop computer.

To reproduce all analyses in the paper, we recommend that you: 

1. Clone the GitHub repository to your computer

For example, in the location on your computer where you would like to clone the repository, you could type into the Terminal command:

`git clone https://github.com/ben-arnold/washb-geopair.git`

2. Recreate the exact package environment using the `renv` package. 

You can do this by opening the R project file ([washb-geopair.Rproj](https://github.com/ben-arnold/washb-geopair/blob/main/washb-geopair.Rproj)) in RStudio, loading the `renv` package, and typing `renv::restore()` to restore the package environment from the projects [renv.lock](https://github.com/ben-arnold/washb-geopair/blob/main/renv.lock) file. 

3. Download the public data from the OSF repository by running the script [`01-washb-geopair-download-public-data.R`](https://github.com/ben-arnold/washb-geopair/blob/main/R/01-washb-geopair-download-public-data.R).
  
4. All of the analysis scripts should run smoothly (scripts `02-xx.Rmd` to `08-xx.Rmd`).  Note that scripts `09-13` and script `04` rely on geographic coordinates of study clusters that constitute personally identifiable information. Therefore, those scripts will not run (GPS data not publicly available), but the scripts have been provided for transparency.

### Additional details

The first data processing script will download harmonized datasets from OSF and will create the final analysis datasets.

The data analyses on the above Mac desktop configuration required 13 minutes to run. 

Note that the only script that takes very long from those that will run on publicly available data (through script `08`) is `05-washb-geopair-estimate-icc.R` because bootstrapping mixed models using REML is computationally slow.

Note, in our final reproducibility run on January 4, 2024, we found that we needed to use the `malariaAtlas` R package version 1.0.1 rather than the most current version 1.5.1 — something has changed in the way that it interacts with the `gdistance` package, when used to estimate the transition matrix for travel times in Bangladesh.  If you use `renv::restore()` as suggested above it will rely on the corrrect package version, though this is inconsequential for public reproducibility since this only affects an analysis that relies on GPS data, which are not public.

### License

This project is covered by the CC0 1.0 Universal license.
