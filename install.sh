#!/bin/bash

# This file is supposed to install dimRed and all dependencies and development
# tools for the R version chosen below.

# R_FOLDER=/usr/bin
# R_FOLDER=$HOME/progs/R/R-3.5.1/bin
# R_FOLDER=$HOME/progs/R/R-3.5.2/bin
# R_FOLDER=$HOME/progs/R/R-3.5.3/bin
R_FOLDER=$HOME/progs/R/R-3.6.0/bin
# R_FOLDER=$HOME/progs/R/R-devel/bin

$R_FOLDER/R --version

pkgversion=$(cat DESCRIPTION | grep Version | sed 's|Version: \(.*\)|\1|')
echo "INSTALLING version $pkgversion"
$R_FOLDER/R CMD INSTALL dimRed_$pkgversion.tar.gz

$R_FOLDER/R -e 'install.packages(c("NMF", "magrittr", "DRR", "lintr", "knitr"), Ncpus = 4, ask = FALSE, repos = "https://cloud.r-project.org")'
$R_FOLDER/R -e 'options(Ncpus = 4, repos = "https://cloud.r-project.org"); dimRed::installSuggests()'
# $R_FOLDER/R -e 'tensorflow::install_tensorflow()'
# $R_FOLDER/R -e 'keras::install_keras()'

pip install --user --upgrade tensorflow keras umap-learn

$R_FOLDER/R -e 'options(Ncpus = 4, repos = "https://cloud.r-project.org"); install.packages("devtools")'
