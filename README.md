
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stcsanalysis

<!-- badges: start -->
<!-- badges: end -->

The stcsanalysis package contains function to handle the STCS analysis
tables (AT).

## Installation

Choose the correct version of stcsanalysis depending on the version of
AT:

For the latest version (from 0.0.5) of the AT , you can install
stcsanalysis like so:

``` r
remotes::install_github("swiss-transplant-cohort-study/stcsanalysis")
```

For version 0.0.4 (and earlier) of the AT, you can install stcsanalysis
like so:

``` r
remotes::install_github("swiss-transplant-cohort-study/stcsanalysis@at-version-0.0.4")
```

For version 0.0.0 of the AT, you can install stcsanalysis like so:

``` r
remotes::install_github("swiss-transplant-cohort-study/stcsanalysis@at-version-0.0.0")
```

## Example

Access the tailored tables using:

``` r
library(stcsanalysis)
stcs <- stcs_read("dir/to/analysis/tables")

patient_bl <- tailored_patientbl(stcs)
patient_surv <- tailored_patientsurvival(stcs)
```
