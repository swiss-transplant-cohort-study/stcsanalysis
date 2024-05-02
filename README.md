
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stcsanalysis

<!-- badges: start -->
<!-- badges: end -->

The stcsanalysis package contains function to handle the STCS analysis
tables (AT).

## Installation

Choose the correct version of stcsanalysis depending on the version of
AT:

For the latest version (0.0.1) of the AT , you can install stcsanalysis
like so:

``` r
remotes::install_github("swiss-transplant-cohort-study/stcsanalysis")
```

For version 0.0.0 of the AT, you can install stcsanalysis like so:

``` r
remotes::install_github("swiss-transplant-cohort-study/stcsanalysis@at-version-0.0.0")
```

## Example

You can create a table using:

``` r
library(stcsanalysis)
stcs <- stcs_read("dir/to/analysis/tables")

data_patientkey(stcs) |> 
  add_var(stcs,c("enrollment_age"))|> 
  expand_var_from(stcs,from="transplantation",by="patientkey",.var="soaskey")|> 
  expand_var_from(stcs,from="organ",by="soaskey",.var="organkey")|> 
  add_var(stcs,c("organ","tpxdate"))|> 
  add_var(stcs,"deathdate",from = "stop", by = "patientkey",.filter = !is.na(deathdate))
```
