enaR: Tools for Ecological Network Analysis
===========================================

Build Status
============

 | Branch      |Status                                                                                                      |
 |-------------|------------------------------------------------------------------------------------------------------------|
 | master      | [![Build Status](https://travis-ci.org/SEELab/enaR.svg?branch=master)](https://travis-ci.org/SEELab/enaR)  |
 | development | [![Build Status](https://travis-ci.org/SEELab/enaR.svg?branch=develop)](https://travis-ci.org/SEELab/enaR) |


[enaR](https://cran.r-project.org/package=enaR) provides a set of
high-level functions for conducting ecological network analysis
(ENA). These functions allow users to access the many tools developed
over decades of work by ecologists looking for ways to measure aspects
of the struture and functioning of complex ecological systems, such as
food-webs or biogeochemical cycles. 

In addition to collecting the many ENA algorithms together into a
single, open-source toolbox,
[enaR](https://cran.r-project.org/package=enaR) also facilitates the
import, construction and simulation of ecological network
models. There are multiple functions for reading in data from many of
the various model formats that have arisen over the years.


To Install and Load
===================

```R
install.packages("enaR")
library(enaR)
```

You can also get the current development version directly from Github:

```R
install.packages("devtools")
devtools::install_github("ProvTools/ProvR")
library(enaR)
```

Basic Usage
===========

Load a model from our set of models included with the package and
generate the full set of ENA metrics and indictors:

```R
data(enaModels)
model <- enaModels[[8]]
model.ena <- enaAll(model)
```

You can now explore the many metrics produced for the model, such as
structural aspects of the model:

```R
model.ena$structure
View(model.ena$structure)
```

For a more in-depth introduction to ENA and how to use the
[enaR](https://cran.r-project.org/package=enaR) package, you can view
our extensive [vignette](http://198.199.73.21:3838/ena-tutorial/)
(http://198.199.73.21:3838/ena-tutorial/).

