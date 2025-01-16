#' Dimensionality Reduction via Regression
#'
#' An S4 Class implementing Dimensionality Reduction via Regression (DRR).
#'
#' DRR is a non-linear extension of PCA that uses Kernel Ridge regression.
#'
#' @template dimRedMethodSlots
#'
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#' DRR can take the following parameters:
#' \describe{
#'   \item{ndim}{The number of dimensions}
#'   \item{lambda}{The regularization parameter for the ridge
#'   regression.}
#'   \item{kernel}{The kernel to use for KRR, defaults to
#'   \code{"rbfdot"}.}
#'   \item{kernel.pars}{A list with kernel parameters, elements depend
#'   on the kernel used, \code{"rbfdot"} uses \code{"sigma"}.}
#'   \item{pca}{logical, should an initial pca step be performed,
#'   defaults to \code{TRUE}.}
#'   \item{pca.center}{logical, should the data be centered before the
#'   pca step. Defaults to \code{TRUE}.}
#'   \item{pca.scale}{logical, should the data be scaled before the
#'   pca ste. Defaults to \code{FALSE}.}
#'   \item{fastcv}{logical, should \code{\link[CVST]{fastCV}} from the
#'   CVST package be used instead of normal cross-validation.}
#'   \item{fastcv.test}{If \code{fastcv = TRUE}, separate test data set for fastcv.}
#'   \item{cv.folds}{if \code{fastcv = FALSE}, specifies the number of
#'   folds for crossvalidation.}
#'   \item{fastkrr.nblocks}{integer, higher values sacrifice numerical
#'   accuracy for speed and less memory, see below for details.}
#'   \item{verbose}{logical, should the cross-validation results be
#'   printed out.}
#' }
#'
#' @section Implementation:
#' Wraps around \code{\link[DRR]{drr}}, see there for details. DRR is
#' a non-linear extension of principal components analysis using Kernel
#' Ridge Regression (KRR, details see \code{\link[CVST]{constructKRRLearner}}
#' and \code{\link[DRR]{constructFastKRRLearner}}). Non-linear
#' regression is used to explain more variance than PCA. DRR provides
#' an out-of-sample extension and a backward projection.
#'
#' The most expensive computations are matrix inversions therefore the
#' implementation profits a lot from a multithreaded BLAS library.
#' The best parameters for each KRR are determined by cross-validaton
#' over all parameter combinations of \code{lambda} and
#' \code{kernel.pars}, using less parameter values will speed up
#' computation time. Calculation of KRR can be accelerated by
#' increasing \code{fastkrr.nblocks}, it should be smaller than
#' \eqn{n^{1/3}} up to sacrificing some accuracy, for details see
#' \code{\link[DRR]{constructFastKRRLearner}}. Another way to speed up
#' is to use \code{pars$fastcv = TRUE} which might provide a more
#' efficient way to search the parameter space but may also miss the
#' global maximum, I have not ran tests on the accuracy of this method.
#'
#'
#'
#' @references
#' Laparra, V., Malo, J., Camps-Valls, G.,
#'     2015. Dimensionality Reduction via Regression in Hyperspectral
#'     Imagery. IEEE Journal of Selected Topics in Signal Processing
#'     9, 1026-1036. doi:10.1109/JSTSP.2015.2417833
#'
#' @examples
#' \dontrun{
#' if(requireNamespace(c("kernlab", "DRR"), quietly = TRUE)) {
#'
#' dat <- loadDataSet("variable Noise Helix", n = 200)[sample(200)]
#'
#' emb <- embed(dat, "DRR", ndim = 3)
#'
#' plot(dat, type = "3vars")
#' plot(emb, type = "3vars")
#'
#' # We even have function to reconstruct, also working for only the first few dimensions
#' rec <- inverse(emb, getData(getDimRedData(emb))[, 1, drop = FALSE])
#' plot(rec, type = "3vars")
#' }
#'
#' }
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @import DRR
#' @family dimensionality reduction methods
#' @export DRR
#' @exportClass DRR
DRR <- setClass(
    "DRR",
    contains  = "dimRedMethod",
    prototype = list(
        stdpars = list(ndim            = 2,
                       lambda          = c(0, 10 ^ (-3:2)),
                       kernel          = "rbfdot",
                       kernel.pars     = list(sigma = 10 ^ (-3:4)),
                       pca             = TRUE,
                       pca.center      = TRUE,
                       pca.scale       = FALSE,
                       fastcv          = FALSE,
                       cv.folds        = 5,
                       fastcv.test     = NULL,
                       fastkrr.nblocks = 4,
                       verbose         = TRUE),
        fun = function (data, pars,
                        keep.org.data = TRUE) {
        chckpkg("DRR")
        chckpkg("kernlab")


        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        indata <- data@data

        res <- do.call(DRR::drr, c(list(X = indata), pars))

        outdata <- res$fitted.data
        colnames(outdata) <- paste0("DRR", 1:ncol(outdata))

        appl <- function(x){
            appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
            proj <- if (inherits(x, "dimRedData")) x@data else x

            if (ncol(proj) != ncol(data@data))
                stop("x must have the same number of dimensions ",
                     "as the original data")

            appl.out <- new("dimRedData",
                            data = res$apply(proj),
                            meta = appl.meta)
            dimnames(appl.out@data) <- list(
                rownames(x), paste0("DRR", seq_len(ncol(appl.out@data)))
            )
            return(appl.out)
        }

        inv <- function(x) {
            appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
            proj <- if (inherits(x, "dimRedData")) x@data else x

            if (ncol(proj) > ncol(data@data))
                stop("x must have less or equal number of dimensions ",
                     "as the original data")

            inv.out <- new("dimRedData",
                           data = res$inverse(proj),
                           meta = appl.meta)
            dimnames(inv.out@data) <- list(rownames(proj), colnames(data@data))
            return(inv.out)
        }


        return(
            new("dimRedResult",
                data = new("dimRedData",
                           data = outdata,
                           meta = meta),
                org.data = orgdata,
                apply = appl,
                inverse = inv,
                has.org.data = keep.org.data,
                has.apply = TRUE,
                has.inverse = TRUE,
                method = "drr",
                pars = pars
                )
        )
        },
      requires = c("DRR", "kernlab"))
)
