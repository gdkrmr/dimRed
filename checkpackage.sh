#!/bin/bash

echo "BUILDING DOCUMENTATION"
Rscript -e 'devtools::document()'

echo "REMOVING emacs lockfiles"
find . -type l -exec rm {} \;

echo "BUILDING"
R CMD build .

pkgversion=$(cat DESCRIPTION | grep Version | sed 's|Version: \(.*\)|\1|')
echo "INSTALLING version $pkgversion"
R CMD INSTALL dimRed_$pkgversion.tar.gz

echo "LINTING"
Rscript -e 'lintr::lint_package()'

echo ""
echo 'CHECKING AS CRAN!!!'
R CMD check dimRed_$pkgversion.tar.gz --as-cran --timings

echo 'CHECK everything!!!'
R CMD check dimRed_$pkgversion.tar.gz --run-donttest --run-dontrun --timings
# echo "INSTALLING on cluster!"
# ssh pc026 R_LIBS=/User/homes/gkraemer/.R_libs /usr/local/apps/R/R-3.2.2/bin/R CMD INSTALL gkraemer/progs/dimRed/dimRed_$pkgversion.tar.gz
