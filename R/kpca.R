#' Kernel PCA
#'
#' An S4 Class implementing Kernel PCA
#'
#' Kernel PCA is a nonlinear extension of PCA using kernel methods.
#'
#'
#' @template dimRedMethodSlots
#'
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#' Kernel PCA can take the following parameters:
#' \describe{
#'   \item{ndim}{the number of output dimensions, defaults to 2}
#'   \item{kernel}{The kernel function, either as a function or a
#'   character vector with the name of the kernel. Defaults to
#'   \code{"rbfdot"}}
#'   \item{kpar}{A list with the parameters for the kernel function,
#'     defaults to \code{list(sigma = 0.1)}}
#' }
#'
#' The most comprehensive collection of kernel functions can be found in
#' \code{\link[kernlab]{kpca}}. In case the function does not take any
#' parameters \code{kpar} has to be an empty list.
#'
#' @section Implementation:
#'
#' Wraps around \code{\link[kernlab]{kpca}}, but provides additionally
#' forward and backward projections.
#'
#' @references
#'
#' Sch\"olkopf, B., Smola, A., M\"uller, K.-R., 1998. Nonlinear Component Analysis
#' as a Kernel Eigenvalue Problem. Neural Computation 10, 1299-1319.
#' https://doi.org/10.1162/089976698300017467
#'
#' @examples
#' \dontrun{
#' if(requireNamespace("kernlab", quietly = TRUE)) {
#'
#' dat <- loadDataSet("3D S Curve")
#' emb <- embed(dat, "kPCA")
#' plot(emb, type = "2vars")
#' }
#'
#' }
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @export kPCA
#' @exportClass kPCA
kPCA <- setClass(
    "kPCA",
    contains = "dimRedMethod",
    prototype = list(
        stdpars = list(kernel = "rbfdot",
                       kpar = list(sigma = 0.1),
                       ndim = 2),
        fun = function (data, pars,
                        keep.org.data = TRUE) {

        chckpkg("kernlab")

        if (is.null(pars$ndim)) pars$ndim <- 2

        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        indata <- data@data

        message(Sys.time(), ": Calculating kernel PCA")
        res <- do.call(kernlab::kpca, c(list(x = indata), pars))

        kernel <- get_kernel_fun(pars$kernel, pars$kpar)

        message(Sys.time(), ": Trying to calculate reverse")
        K_rev <- kernlab::kernelMatrix(kernel, res@rotated)
        diag(K_rev) <- 0.1 + diag(K_rev)
        dual_coef <- try(solve(K_rev, indata), silent = TRUE)

        appl <- function (x) {
            appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
            proj <- if (inherits(x, "dimRedData")) x@data else x

            proj <- kernlab::predict(res, proj)[, 1:pars$ndim, drop = FALSE]
            colnames(proj) <- paste0("kPCA", 1:ncol(proj))

            new("dimRedData", data = proj, meta = appl.meta)
        }

        inv <-
          if (inherits(dual_coef, "try-error")) {
            message("No inverse function.")
            function(x) NA
          } else {
            function (x) {
              appl.meta <-
                if (inherits(x, "dimRedData")) x@meta else data.frame()
              proj <- if (inherits(x, "dimRedData")) x@data else x

              resrot <- res@rotated[, 1:ncol(proj)]
              rot <- kernlab::kernelMatrix(kernel, proj, resrot)
              proj <- rot %*% dual_coef

              new("dimRedData", data = proj, meta = appl.meta)
            }
          }


        outdata <- res@rotated[, 1:pars$ndim, drop = FALSE]
        colnames(outdata) <- paste0("kPCA", 1:ncol(outdata))

        message(Sys.time(), ": DONE")
        return(
            new(
                "dimRedResult",
                data         = new("dimRedData",
                                   data = outdata,
                                   meta = meta),
                org.data     = orgdata,
                apply        = appl,
                inverse      = inv,
                has.org.data = keep.org.data,
                has.apply    = TRUE,
                has.inverse  = TRUE,
                method       = "kpca",
                pars         = pars
            )
        )
        },
      requires = c("kernlab"))
)


## get the kernel function out of the kernlab namespace:
get_kernel_fun <- function (kernel, pars) {
    if (!is(kernel, "kernel")) {
        if (is(kernel, "function")) {
            kernel <- deparse(substitute(kernel))
        } else {
           kernel <- get(kernel, asNamespace("kernlab"))
        }
        kernel <- do.call(kernel, pars)
    }
    return(kernel)
}
