# dimRed
[![Travis Build Status](https://travis-ci.org/gdkrmr/dimRed.svg?branch=master)](https://travis-ci.org/gdkrmr/dimRed) [![Coverage Status](https://img.shields.io/codecov/c/github/gdkrmr/dimRed/master.svg)](https://codecov.io/github/gdkrmr/dimRed?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dimRed)](https://cran.r-project.org/package=dimRed)
A Framework for Dimensionality Reduction for the R language.

A collection of dimensionality reduction
techniques from R packages and provides a common
interface for calling the methods.

## Installing:
```R
## install.packages("devtools")
devtools::install_github("gdkrmr/dimRed")
```

Install from CRAN
```R
## not yet possible, but once it is on there use:
install.packages("dimRed")
```

Load it:
```R
library(dimRed)
```

Install dependencies:
```R
## The package dependencies are not installed automatically:
dimRed::installSuggests()
```

