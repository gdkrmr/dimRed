#' Principal Component Analysis with L1 error.
#'
#' S4 Class implementing PCA with L1 error.
#'
#' PCA transforms the data so that the L2 reconstruction error is minimized or
#' the variance of the projected data is maximized. This is sensitive to
#' outliers, L1 PCA minimizes the L1 reconstruction error or maximizes the sum
#' of the L1 norm of the projected observations.
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
#'   \item{fun}{character or function, the method to apply, see the \code{pcaL1} package}
#'   \item{\ldots}{other parameters for \code{fun}}
#' }
#'
#' @section Implementation:
#'
#' Wraps around the different methods is the \code{pcaL1} package. Because PCA
#' can be reduced to a simple rotation, forward and backward projection
#' functions are supplied.
#'
#' @examples
#' dat <- loadDataSet("Iris")
#'
#' ## using the S4 Class
#' pca_l1 <- PCA_L1()
#' emb <- pca_l1@fun(dat, pca_l1@stdpars)
#'
#' ## using embed()
#' emb2 <- embed(dat, "PCA_L1")
#'
#' plot(emb, type = "2vars")
#' plot(emb@inverse(emb@data), type = "3vars")
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @export PCA_L1
#' @exportClass PCA_L1
PCA_L1 <- setClass(
    "PCA_L1",
    contains = "dimRedMethod",
    prototype = list(
        stdpars = list(ndim = 2,
                       center = TRUE,
                       scale. = FALSE,
                       fun = "awl1pca",
                       projections = "l1"),
        fun = function (data, pars,
                        keep.org.data = TRUE) {
        chckpkg("pcaL1")

        ndim <- pars$ndim

        orgnames <- colnames(data@data)
        newnames <- paste0("PC", seq_len(ndim))

        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        data <- data@data

        fun2 <- if(!is.function(pars$fun)) {
                    get(pars$fun, asNamespace("pcaL1"))
                } else {
                    pars$fun
                }

        ce <- if (is.numeric(pars$center)) {
                  if (length(pars$center) != dim(data)[2])
                      error("center must be logical or have the same length as the data dimensions")
                  pars$center
              } else if (is.logical(pars$center)) {
                  if (pars$center) colMeans(data) else FALSE
              }

        sc <- if (is.numeric(pars$scale.)) {
                  if (length(pars$scale.) != dim(data)[2])
                      stop("center must be logical or have the same length as the data dimensions")
                  pars$scale.
              } else if (is.logical(pars$scale.)) {
                  if (pars$scale.) apply(data, 2, sd) else FALSE
              }

        if(!(pars$center == FALSE && pars$scale. == FALSE))
            data <- scale(data, ce, sc)


        pars$center <- NULL
        pars$scale. <- NULL
        pars$ndim <- NULL
        pars$fun <- NULL

        res <- do.call(
            fun2,
            c(list(X = data, projDim = ndim, center = FALSE), pars)
        )

        ## evaluate results here for functions
        data <- res$scores
        colnames(data) <- paste0("PC", seq_len(ndim))
        rot <- res$loadings[, seq_len(ndim)]
        dimnames(rot) <- list(orgnames, newnames)
        rerot <- t(rot)


        appl <- function(x) {
            appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
            proj <- if (inherits(x, "dimRedData")) x@data else x

            if (ncol(proj) != ncol(orgdata))
                stop("x must have the same number of dimensions ",
                     "as the original data")

            if (ce[1]  != FALSE) proj <- t(apply(proj, 1, function(x) x - ce))
            if (sc[1]  != FALSE) proj <- t(apply(proj, 1, function(x) x / sc))

            proj <- if (pars$projections == "l1") {
                        tmp <- pcaL1::l1projection(proj, rot)$scores
                        colnames(tmp) <- paste0("PC", seq_len(ndim))
                        tmp
                    } else if (pars$projections == "l2") {
                        proj %*% rot
                    } else {
                        stop("projections must be eiter 'l1' or 'l2'")
                    }

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

            colnames(reproj) <- colnames(orgdata)

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
            method       = "PCA_L1",
            pars         = pars
        )

        return(res)
    })
)
