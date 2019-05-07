
Reassessing the Evidence for Universal School-age Bacillus Calmette Guerin (BCG) Vaccination in England and Wales
=================================================================================================================

[![badge](https://img.shields.io/badge/Launch-Analysis-lightblue.svg)](https://mybinder.org/v2/gh/seabbs/AssessBCGPolicyChange/master?urlpath=rstudio) [![Documentation](https://img.shields.io/badge/Documentation-click%20here!-lightgrey.svg?style=flat)](https://www.samabbott.co.uk/AssessBCGPolicyChange) [![Paper](https://img.shields.io/badge/Paper-10.1101/624916-lightgreen.svg)](https://doi.org/10.1101/624916) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2635687.svg)](https://doi.org/10.5281/zenodo.2635687)

[Sam Abbott](https://www.samabbott.co.uk), Hannah Christensen, Ellen Brooks-Pollock

Background
----------

In 2005, England and Wales switched from universal BCG vaccination against tuberculosis (TB) disease for school-age children to targeted vaccination of neonates. We assessed the quantitative evidence that informed this policy change.

Methods
-------

We recreated a previous approach for estimating the impact of ending the BCG schools’ scheme in England and Wales, updating the model with parameter uncertainty. We investigated scenarios considered by the UK’s Joint Committee on Vaccination and Immunisation, and explored new approaches using notification data. We estimated the number of vaccines needed to prevent a single notification, and the average annual additional notifications caused by ending the BCG schools’ scheme.

Results
-------

We found a 1.9% annual decrease in TB incidence rates best matched notification data. We estimate that 1600 (2.5-97.5% Quantiles (Q): 1300 - 2100) vaccines would have been required to prevent a single notification in 2004. If the scheme had ended in 2001, 302 (2.5-97.5% Q: 238 - 369) additional annual notifications would have occurred compared to if the scheme had continued. If the scheme ended in 2016, 120 (2.5-97.5% Q: 88 - 155) additional annual notifications would have occurred.

Conclusions
-----------

Our estimates of the impact of ending the BCG schools’ scheme were highly sensitive to the annual decrease in incidence rates. The impact of ending the BCG schools’ scheme was found to be greater than previously thought when parameter values were updated and notification data were used. Our results highlight the importance of including uncertainty when forecasting the impact of changes in vaccination policy.

Reproducibility
---------------

### Repository structure

The repository is structured as an R package. It has the following structure:

-   `data-raw`: Raw data processing.
-   `data`: Processed data.
-   `R`: Supporting R functions.
-   `docs:` Documentation for R code.
-   `vignettes`: Analysis paper, results, and analysis plan.
-   `peer-review`: Documentation required for peer review.

### Manual install

-   Install R (analysis run with `3.5.2`) and Rstudio (alternatively use Docker as outlined below).

-   Download the analysis folder from <https://github.com/seabbs/AssessBCGPolicyChange/archive/master.zip> or use `git clone`, as follows, in the command line (not the R terminal).

``` bash
git clone https://github.com/seabbs/AssessBCGPolicyChange.git
```

-   Once this has been downloaded click on the project file (`AssessBCGPolicyChange.Rproj`).

-   Install the analysis dependencies and build the package using the following. To enable more robust reproducibility consider using the [`checkpoint`](https://cran.r-project.org/web/packages/checkpoint/index.html) package versioned locked to R `3.5.2`.

``` r
#install.packages("devtools")
# To build locally
devtools::install_dev_deps(dependencies = TRUE)
devtools::install()
# Alternatively to remote install
devtools::install_github("seabbs/AssessBCGPolicyChange", dependencies = TRUE)
```

-   Load the analysis results by running `vignettes/paper.Rmd`. Alternatively the complete analysis (along with documentation) can be reconstructed using `make` in the project root directory.

-   See `data-raw` for data processing and the documentation for implementation details.

### Docker

This analysis was developed in a docker container based on the tidyverse docker image. To run the docker image run:

``` bash
docker run -d -p 8787:8787 --name assessbcgpolicychange -e USER=assessbcgpolicychange -e PASSWORD=assessbcgpolicychange seabbs/assessbcgpolicychange
```

The rstudio client can be found on port :8787 at your local machines ip. The default username:password is assessbcgpolicychange:assessbcgpolicychange, set the user with -e USER=username, and the password with - e PASSWORD=newpasswordhere. The default is to save the analysis files into the user directory.

If you have access to the required underlying raw data (see [`tbinenglanddataclean`](https://www.samabbott.co.uk/tbinenglanddataclean/)) then the entire analysis can be reproduced from scratch by adding the following to the `docker run` command, with the data saved into `data/tb_data`. The data requirements, and structure, can be found [here](https://www.samabbott.co.uk/tbinenglanddataclean/).

``` bash
--mount type=bind,source=$(pwd)/data/tb_data,target=/home/DirectEffBCGPolicyChange/data/tb_data
```

Alternatively the analysis environment can be accessed via [binder](https://mybinder.org/v2/gh/seabbs/AssessBCGPolicyChange/master?urlpath=rstudio).
