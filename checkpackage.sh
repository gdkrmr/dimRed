#!/bin/bash

# R_FOLDER=/usr/bin
# R_FOLDER=$HOME/progs/R/R-3.5.1/bin
R_FOLDER=$HOME/progs/R/R-devel/bin

$R_FOLDER/R --version

echo "BUILDING DOCUMENTATION"
Rscript --vanilla --default-packages=methods,utils \
        -e 'devtools::document()'

echo "REMOVING emacs lockfiles"
find . -type l -exec rm {} \;

echo "BUILDING"
# build_vignette ()
# {
#     local BNET_BUILD_VIGNETTE=1
#     R CMD build --compact-vignettes .
# }

# build_vignette

# Setting this forces the testing of the autoencoders, even if there is no
# tensorflow or keras installed.
BNET_FORCE_AUTOENCODER_TESTS=1
BNET_FORCE_UMAP_TESTS=1

BNET_BUILD_VIGNETTE=1 $R_FOLDER/R CMD build --compact-vignettes .

pkgversion=$(cat DESCRIPTION | grep Version | sed 's|Version: \(.*\)|\1|')
echo "INSTALLING version $pkgversion"
$R_FOLDER/R CMD INSTALL dimRed_$pkgversion.tar.gz

echo "LINTING"
$R_FOLDER/Rscript -e 'lintr::lint_package()'

echo ""
echo "CHECKING AS CRAN!!!"
$R_FOLDER/R CMD check dimRed_$pkgversion.tar.gz --as-cran --timings

echo "CHECK everything!!!"
$R_FOLDER/R CMD check dimRed_$pkgversion.tar.gz --run-donttest --run-dontrun --timings
