
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ssoestimate

<!-- badges: start -->
<!-- badges: end -->

The goal of ssoestimate is to … \# steek SSO \# \# Stel, je hebt een
shared serviceorganisatie, SSO, die boekhouding uitvoert voor \#
verschillende klanten. \# Voor elk van die klanten moet worden bekeken
of die boekhouding door \# de SSO voldoende is. \# Hoe mooi zou het
zijn, als je nu 1 keer een controle uitvoert bij de SSO \# en daarmee
voldoende zekerheid krijgt over de boekhouding van elk van de klanten?
\# Dat proberen we hier handen en voeten te geven. \# \# De methode die
we daarvoor gebruiken heeft één hele sterke aanname nodig namelijk: \#
de verzameling posten per klant, over een boekhoudperiode, \# is een
steekproef over alle posten van alle klanten van de SSO, over die
periode. \# \# We willen die ene controle bij de SSO uitvoeren als een
steekproef. \# We beschouwen dan twee steekproeven: \# - een steekproef,
Steek_SSO, \# van n_SSO posten over de m posten van alle klanten van de
SSO, en \# - een steekproef, Steek_client, van n_client posten zijnde
alle posten van 1 klant van de SSO, \# ook deze steekproef is, zo nemen
we dus aan, over alle m posten van alle klanten van de SSO. \# \# Beide
steekproeven worden dus uit dezelfde verzameling getrokken, \# namelijk
de verzameling van alle m posten van alle klanten van de SSO. \# \# Nu
moeten we bepalen wat de uitkomst van de steekproef van n_SSO posten ons
kan \# vertellen over de uitkomst van de steekproef over n_client
posten. \# \# We proberen, zeg maar, de afstand tussen de resultaten van
twee steekproeven \# op dezelfde massa te schatten. Die afstand zal
kleiner worden naarmate n_SSO en \# n_client groter worden. \# \# Ons
doel is dus om de kanskromme van de foutfractie in Steek_client (dus
foutfractie van \# alle n_client posten van de klant, volgens onze
aanname) te schatten. \# Laten we die foutfractie p_client noemen, en de
kanskromme daarvan P_client. \# n_client is bekend. \# Wat niet bekend
is, is het aantal fouten k_client in Steek_client en dus \# ook de
fractie p_client = k_client/n_client. \# \# We gaan er nu vanuit dat we
de steekproef op de gehele massa m van de SSO \# hebben uitgevoerd. We
kennen dus het aantal getrokken posten, n_SSO, en \# en het aantal
gevonden fouten, k_SSO, van Steek_SSO. \# \# We gebruiken als basis voor
het algoritme gepostuleerde mogelijke \# waarden voor p, de kans op
fouten in posten van m. \# We berekenen in het algoritme voor een geheel
rooster van waarden van p over \[0, 1\], \# de kans op k_client,
p_client dus. \# In dit geval is het ‘rooster’ 1-dimensionaal, dus het
gaat om punten die op een lijn liggen. \# \# In plaats van over een heel
rooster te itereren zouden we ook \# met Monte Carlo, kunnen werken, dus
bij toeval trekken. \# Mogelijk kunnen we ook Gibbs sampling of
Metropolis gebruiken, \# met de hand uit te programmeren, of via BUGS of
stan. \# Bij een veeldimensionaal rooster is dat waarschijnlijk een
betere aanpak. \# \# Eerst presenteren we een algoritme, SSO_estimate,
waarbij we werken met alleen gehele \# waarden voor k_client. Daarna
presenteren we \# SSO_estimate_continuous, dat een ietsje complexere
variant is van SSO_estimate. \# SSO_estimate_continuous kan namelijk ook
werken met nietgehele waarden voor k_client.

## Installation

You can install the development version of ssoestimate like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
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
