
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ssoestimate

<!-- badges: start -->

[![R-CMD-check](https://github.com/cfjdoedens/ssoestimate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cfjdoedens/ssoestimate/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A shared Service Organsition, SSO, is an administrative entity that
carries out financial transaction on behalf of a number of client
organisations. The kind of transactions done by the SSO are the same for
all clients.

The goal of ssoestimate is to estimate the monetary error rate of
transactions for each single client of the SSO. This is done by using a
sample from all the transactions of the SSO as a proxy. This assumes
that the most efficient way to estimate this error rate is by means of
sampling: it is not feasable to do for example an integral data-analyses
of the transactions. The one big assumption that ssoestimate uses is
that the set of transactions from the client is a random sample from all
the transactions carried out by the SSO.

Using this assumption makes it possible to efficiently estimate the
error rate for all the clients of the SSO. For example suppose that the
SSO processed 100,000 transactions, for 10 clients. And suppose we want
to establish with 95% confidence that the error rate for each client is
at most 1%, then we could use (expecting no errors in the samples) a
sample of 300 transactions from each client, totalling 3000 samples.
Instead when using ssoestimate it suffices to draw only 350 transactions
in total to conclude with 95% confidence that the error rate of each
client is at most 1%.

A small paper about the algorithm used is forthcoming.

## Installation

You can install the development version of ssoestimate like so:

``` r
install.packages("devtools")
devtinstall_github("cfjdoedens/ssoestimate")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ssoestimate)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
