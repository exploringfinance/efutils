
<!-- README.md is generated from README.Rmd. Please edit that file -->

# efutils

<!-- badges: start -->

![Dev Version](https://img.shields.io/badge/github-0.0.1-blue.svg)
<!-- badges: end -->

The goal of efutils is to provide Utility functions used by [Exploring
Finance](https://exploringfinance.github.io/).

## Installation

This will not be submitted to CRAN. Install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("exploringfinance/efutils")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(efutils)
## basic example code

ef_conv_perc(.1234)
#> [1] "12.34%"

ef_date_diff('2020-01-01','2020-12-31',bus_only = TRUE)
#> Warning: 1 parsing failure.
#> row col expected actual
#>   1  -- a number     —*
#> Warning: 1 parsing failure.
#> row col expected actual
#>   1  -- a number      —
#> [1] 252

ef_if.na(NA,3)
#> [1] 3
```
