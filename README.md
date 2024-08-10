
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ssoestimate

<!-- badges: start -->

[![R-CMD-check](https://github.com/cfjdoedens/ssoestimate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cfjdoedens/ssoestimate/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

A shared Service Organisation, SSO, is an administrative entity that
carries out financial transaction on behalf of a number of client
organisations. The kind of transactions done by the SSO are the same for
all clients.

The goal of ssoestimate is to estimate the monetary error rate of
transactions for each single client of the SSO. This is done by using a
sample from all the transactions of the SSO as a proxy. This assumes
that the most efficient way to estimate this error rate is by means of
sampling: it is not feasible to do for example an integral data-analyses
of the transactions. The one big assumption that ssoestimate uses is
that the set of transactions from the client is a random sample from all
the transactions carried out by the SSO.

Using this assumption makes it possible to efficiently estimate the
error rate for all the clients of the SSO. For example suppose that the
SSO processed 100,000 transactions, for 10 clients. And suppose we want
to establish with 95% confidence that the error rate for each client is
at most 1%, then we could use (expecting no errors in the samples) a
sample of 300 transactions from each client, totalling 3000
transactions. Instead when using ssoestimate it suffices to draw only
350 transactions in total to conclude with 95% confidence that the error
rate of each client is at most 1%. See the example below.

A small paper about the algorithm used is forthcoming.

## Installation

You can install the development version of ssoestimate like so:

``` r
install.packages("devtools")
devtools::install_github("cfjdoedens/ssoestimate")
```

## Example: visual presentation

This example shows that indeed 350 transactions (n_SSO = 350), which
turn out to have no errors (k_SSO = 0), suffice to ascertain that a
client which has 1000 transactions (N_client = 1000) has with 95% (cert
= 0.95) certainty at most 1% errors (client max 0.010).

The estimated values for SSO max, SSO post probable, SSO min, and ditto
for the client are rounded based on the value of S. The precision of the
estimation is at most 1/(2\*S). So, a higher value for S gives a better
precision of the estimation. In this example the precision is at most
1/2000 = 0.0005.

``` r
library(ssoestimate)
x <- SSO_estimate(k_SSO = 0, n_SSO = 350, N_client = 1000, S = 1000)
SSO_graph_plot(x, cert = 0.95, visual = TRUE)
```

<img src="man/figures/README-visual example-1.png" width="100%" />

## Example: non visual presentation

We can show the result of the estimation carreid out in teh previous
example also non visual. The numbers shown in the non visual
presentation are the raw results of the estimation. The numbers differ
slightly from the visual example, as in the visual example the numbers
are not rounded.

``` r
SSO_graph_plot(x, cert = 0.95, visual = FALSE)
#>          SSO_max    SSO_most_prob          SSO_min       client_max 
#>           0.0085           0.0005           0.0005           0.0095 
#> client_most_prob       client_min 
#>           0.0005           0.0005
```

As we can see, the raw results give a slightly misleading impression, as
we know for example, that the most likely error rate for both SSO and
client is 0.0, and not 0.0005.
