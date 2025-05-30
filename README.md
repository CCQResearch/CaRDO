
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CaRDO

<!-- badges: start -->
<!-- badges: end -->

CaRDO is a user-friendly R package for creating interactive R-Shiny
dashboards that visualize and publish population-level cancer
statistics. You can find an example CaRDO dashboard
<a href="https://cancercouncilqueensland.shinyapps.io/CaRDOExample/"
target="_blank">here</a>

Detailed documentation for building a CaRDO dashboard is available
<a href="https://ccqresearch.github.io/CaRDO-Handbook/"
target="_blank">here</a>

## Installation

1.  CaRDO requires the following packages installed before launching

``` r
install.packages("remotes")
install.packages("tidyverse")
install.packages("plotly")
install.packages("markdown")
```

2.  Install CaRDO using

``` r
remotes::install_github("https://github.com/CCQResearch/CaRDO", dependencies = TRUE)
```

3.  Load CaRDO using

``` r
CaRDO::create_dashboard()
```

## Data requirements

There are **three** key requirements for any cancer dataset that is
loaded into CaRDO.

1.  You must have a single column for each variable and outcome you wish
    to report, and each row in your dataset should correspond to a
    unique combination of each variable.
2.  Cancer-type values must be coded as you wish them to be displayed
3.  Cancer counts and any population data must be aggregated by 5-year
    age groups, with age groups coded numerically from 1 – 18.

Further details on data requirements and building a CaRDO dashboard are
available [here](https://ccqresearch.github.io/CaRDO-Handbook/). Please
reach out to us at <statistics@qldcancer.org.au> if you have any
questions or concerns.

## Disclaimer

Data loaded into CaRDO is stored locally on your computer, and all
analyses are performed locally. Your data will not leave your computer
while using CaRDO – CaRDO has been designed with data privacy as a top
priority.

However, if you choose to *publish* your dashboard (e.g., share it
online), data will be uploaded to the cloud at the resolution that it
appears in the dashboard. It is your responsibility to ensure that all
displayed data is appropriate for sharing before publishing publicly.
