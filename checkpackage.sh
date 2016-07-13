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

echo "CHECKING!!!"
R CMD check dimRed_$pkgversion.tar.gz
