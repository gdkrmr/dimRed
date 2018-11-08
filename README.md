# dimRed
[![Travis Build Status](https://travis-ci.org/gdkrmr/dimRed.svg?branch=master)](https://travis-ci.org/gdkrmr/dimRed) 
[![Coverage Status](https://img.shields.io/codecov/c/github/gdkrmr/dimRed/master.svg)](https://codecov.io/github/gdkrmr/dimRed?branch=master) 
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dimRed)](https://cran.r-project.org/package=dimRed)

A Framework for Dimensionality Reduction for the R language.

A collection of dimensionality reduction
techniques from R packages and provides a common
interface for calling the methods.

## Installing
Install the latest development version from Github:
```R
## install.packages("devtools")
devtools::install_github("gdkrmr/dimRed")
```

Install the latest stable version from CRAN:
```R
install.packages("dimRed")
```

Load it:
```R
library(dimRed)
```

Install dependencies:
```R
## To install all dependencies:
dimRed::installSuggests()
```

## Citing
The corresponding publication can be found
[here](https://journal.r-project.org/archive/2018/RJ-2018-039/index.html "dimRed
and coRanking - Unifying Dimensionality Reduction in R"), please cite if you use
`dimRed`:

```bibtex
@article{RJ-2018-039,
  author = {Guido Kraemer and Markus Reichstein and Miguel D. Mahecha},
  title = {{dimRed and coRanking---Unifying Dimensionality Reduction in R}},
  year = {2018},
  journal = {{The R Journal}},
  url = {https://journal.r-project.org/archive/2018/RJ-2018-039/index.html},
  pages = {342--358},
  volume = {10},
  number = {1}
}
```
