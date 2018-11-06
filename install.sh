#!/bin/bash

# R_FOLDER=/usr/bin
# R_FOLDER=$HOME/progs/R/R-3.5.1/bin
R_FOLDER=$HOME/progs/R/R-devel/bin

$R_FOLDER/R --version

pkgversion=$(cat DESCRIPTION | grep Version | sed 's|Version: \(.*\)|\1|')
echo "INSTALLING version $pkgversion"
$R_FOLDER/R CMD INSTALL dimRed_$pkgversion.tar.gz

$R_FOLDER/R -e 'install.packages(c("NMF", "magrittr", "DRR", "lintr"), Ncpus = 4, ask = FALSE)'
$R_FOLDER/R -e 'option(Ncpus = 4); dimRed::installSuggests()'
$R_FOLDER/R -e 'tensorflow::install_tensorflow()'
$R_FOLDER/R -e 'keras::install_keras()'
