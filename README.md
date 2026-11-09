# UmarDataBotR

<!-- badges: start -->
[![R-CMD-check](https://github.com/majazaloznik/UmarDataBotR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/majazaloznik/UmarDataBotR/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/majazaloznik/UmarDataBotR/graph/badge.svg)](https://app.codecov.io/gh/majazaloznik/UmarDataBotR)
<!-- badges: end -->

One mechanism for timestamped logging, error-tolerant steps and
email routing in UMAR's scheduled runners. Failures go to the maintainer,
data-update notices go to users, and weekly or monthly jobs send a
heartbeat so that a run that never happened is noticed.

## Installation

You can install the development version of UMARbotR from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("majazaloznik/UmarDataBotR")
```


