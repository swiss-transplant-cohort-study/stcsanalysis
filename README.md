
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stcswrangling2

<!-- badges: start -->
<!-- badges: end -->

The stcswrangling2 package contains function to handle the STCS analysis
tables.

## Installation

You can install the development version of stcswrangling2 like so:

``` r
remotes::install_github("swiss-transplant-cohort-study/stcswrangling2")
```

## Example

You can create a table using:

``` r
library(stcswrangling2)
stcs <- stcs_read("dir/to/analysis/tables")

data_patientkey(stcs) |> 
  add_var(stcs,c("age"))|> 
  expand_var_from(stcs,from="transplantation",by="patientkey",.var="soaskey")|> 
  expand_var_from(stcs,from="organ",by="soaskey",.var="organkey")|> 
  add_var(stcs,c("organ","tpxdate"))|> 
  add_var(stcs,"deathdate",from = "stop", by = "patientkey",.filter = !is.na(deathdate))
```
