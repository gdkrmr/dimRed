#' Principal Component Analysis
#'
#' S4 Class implementing PCA.
#'
#' PCA transforms the data in orthogonal components so that the first
#' axis accounts for the larges variance in the data, all the
#' following axes account for the highest variance under the
#' constraint that they are orthogonal to the preceding axes.  PCA is
#' sensitive to the scaling of the variables. PCA is by far the
#' fastest and simples method of dimensionality reduction and should
#' probably always be applied as a baseline if other methods are tested.
#'
#' @template dimRedMethodSlots
#'
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#' PCA can take the following parameters:
#' \describe{
#'   \item{ndim}{The number of output dimensions.}
#'   \item{center}{logical, should the data be centered, defaults to \code{TRUE}.}
#'   \item{scale.}{logical, should the data be scaled, defaults to \code{FALSE}.}
#' }
#'
#' @section Implementation:
#'
#' Wraps around \code{\link{prcomp}}. Because PCA can be reduced to a
#' simple rotation, forward and backward projection functions are
#' supplied.
#'
#' @references
#'
#' Pearson, K., 1901. On lines and planes of closest fit to systems of points in
#' space. Philosophical Magazine 2, 559-572.
#'
#' @examples
#' dat <- loadDataSet("Iris")
#' emb <- embed(dat, "PCA")
#'
#' plot(emb, type = "2vars")
#' plot(inverse(emb, getDimRedData(emb)), type = "3vars")
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @export PCA
#' @exportClass PCA
PCA <- setClass(
    "PCA",
    contains = "dimRedMethod",
    prototype = list(
        stdpars = list(ndim = 2,
                       center = TRUE,
                       scale. = FALSE),
        fun = function (data, pars,
                        keep.org.data = TRUE) {
        ndim <- pars$ndim
        pars$ndim <- NULL

        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        data <- data@data
        res <- do.call(
            prcomp,
            c(list(x = data), pars)
        )

                                        # evaluate results here for functions
        data <- res$x[, seq_len(ndim), drop = FALSE]
        ce <- res$center
        sc <- res$scale
        rot <- res$rotation[, seq_len(ndim)]
        rerot <- t(rot)


        appl <- function(x) {
            appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
            proj <- if (inherits(x, "dimRedData")) x@data else x

            if (ncol(proj) != ncol(orgdata))
                stop("x must have the same number of dimensions ",
                     "as the original data")


            if (ce[1]  != FALSE) proj <- t(apply(proj, 1, function(x) x - ce))
            if (sc[1]  != FALSE) proj <- t(apply(proj, 1, function(x) x / sc))
            proj <- proj %*% rot

            proj <- new("dimRedData", data = proj, meta = appl.meta)
            return(proj)
        }
        inv  <- function(x) {
            appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
            proj <- if (inherits(x, "dimRedData")) x@data else x
            if (ncol(proj) > ncol(data))
                stop("x must have less or equal number of dimensions ",
                     "as the original data")


            d <- ncol(proj)
            reproj <- proj %*% rerot[seq_len(d), ]

            if (sc[1] != FALSE)
                reproj <- t(apply(reproj, 1, function(x) x * sc))
            if (ce[1] != FALSE)
                reproj <- t(apply(reproj, 1, function(x) x + ce))

            reproj <- new("dimRedData", data = reproj, meta = appl.meta)

            return(reproj)
        }

        res <- new(
            "dimRedResult",
            data         = new("dimRedData",
                               data = data,
                               meta = meta),
            org.data     = orgdata,
            apply        = appl,
            inverse      = inv,
            has.org.data = keep.org.data,
            has.apply    = TRUE,
            has.inverse  = TRUE,
            method       = "PCA",
            pars         = pars
        )

        return(res)
    })
)
