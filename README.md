
<!-- README.md is generated from README.Rmd. Please edit that file -->

# enaR: Tools for Ecological Network Analysis

<!-- badges: start -->

[![pkgdown](https://github.com/SEELab/enaR/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/SEELab/enaR/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

[![DOI](https://zenodo.org/badge/12623293.svg)](https://zenodo.org/badge/latestdoi/12623293)

`enaR` provides a set of high-level functions for conducting ecological
network analysis (ENA). These functions allow users to access the many
tools developed over decades of work by ecologists looking for ways to
measure aspects of the struture and functioning of complex ecological
systems, such as food-webs or biogeochemical cycles.

In addition to collecting the many ENA algorithms together into a
single, open-source toolbox,`enaR` also facilitates the import,
construction and simulation of ecological network models. There are
multiple functions for reading in data from many of the various model
formats that have arisen over the years.

## Installation

You can install the current release `enaR` like so:

``` r
remotes::install_github("SEELab/enaR")
```

## Basic Usage

Load a model from our set of models included with the package and
generate the full set of ENA metrics and indicators:

``` r
library(enaR)
data(enaModels)
model <- enaModels[[8]]
model.ena <- enaAll(model)
```

You can now explore the many metrics produced for the model, such as
structural aspects of the model:

``` r
model.ena$structure
#> $A
#>               PHYTOPLANKTON ZOOPLANKTON PELAGIC FISH BENTHIC FAUNA
#> PHYTOPLANKTON             0           1            0             1
#> ZOOPLANKTON               0           0            1             1
#> PELAGIC FISH              0           0            0             0
#> BENTHIC FAUNA             0           0            0             0
#> DEMERSAL FISH             0           0            0             0
#> BACTERIA                  0           0            0             0
#>               DEMERSAL FISH BACTERIA
#> PHYTOPLANKTON             0        0
#> ZOOPLANKTON               0        1
#> PELAGIC FISH              0        1
#> BENTHIC FAUNA             1        1
#> DEMERSAL FISH             0        1
#> BACTERIA                  0        0
#> 
#> $ns
#>      n L    C  LD ppr lam1A mlam1A rho  R   d no.scc no.scc.big pscc
#> [1,] 6 9 0.25 1.5 NaN     0      6  NA NA 1.5      6          0    0
```

For a more in-depth introduction to ENA and how to use the
[enaR](https://cran.r-project.org/package=enaR) package, you can view
our [website](https://seelab.github.io/enaR/)
