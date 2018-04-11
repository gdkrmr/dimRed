#' Non-Negative Matrix Factorization
#'
#' S4 Class implementing NNMF.
#'
#' NNMF is a method for decomposing a matrix into a smaller
#'  dimension such that the constraint that the data (and the
#'  projection) are not negative is taken into account.
#'
#' @template dimRedMethodSlots
#'
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#' The method can take the following parameters:
#' \describe{
#'   \item{ndim}{The number of output dimensions.}
#'   \item{method}{character, should be either "scd" for (sequential
#'    coordinate-wise descent) or "lee" (for Lee's multiplicative algorithm).}
#'   \item{loss}{character, should be either  "mse" for mean square error
#'    or "mkl" for mean Kullback-Leibler-divergence.}
#'   \item{max.iter}{integer, maximum number of iterations (default: 500).}
#'   \item{n.threads}{integer, number of threads (default: no parallelism)}
#'   \item{verbose}{integer between 0 and 2, for amount of logging. Defaults
#'    to 0L.}
#'   \item{rel.tol}{numeric, convergence criterion that defaults to 1e-04.}
#' }
#'
#' @section Implementation:
#'
#' Wraps around \code{\link[NNLM]{nnmf}}. Note that the estimation uses random
#'  numbers. To create reproducible results, set the random number seed prior
#'  to execution.
#'
#' @references
#'
#' Lee, D.D., Seung, H.S., 1999. Learning the parts of objects by non-negative
#' matrix factorization. Nature 401, 788-791. https://doi.org/10.1038/44565
#'
#' @examples
#' dat <- loadDataSet("Iris")
#'
#' set.seed(4646)
#' factorization <- embed(dat, "NNMF")
#'
#' proj_dat <- factorization@apply(dat)
#'
#' plot(proj_dat@data[, 1], proj_dat@data[, 2])
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @export NNMF
#' @exportClass NNMF
NNMF <- setClass(
  "NNMF",
  contains = "dimRedMethod",
  prototype = list(
    stdpars = list(ndim = 2L,
                   method = "scd",
                   loss = "mse",
                   max.iter = 500L,
                   n.threads = 1L,
                   verbose = 0L,
                   rel.tol = 1e-04),
    fun = function (data, pars, keep.org.data = TRUE) {
      chckpkg("NNLM")

      meta <- data@meta
      orgdata <- if (keep.org.data) data@data else NULL
      data <- data@data
      if (!is.matrix(data))
        data <- as.matrix(data)
      if (pars$ndim > ncol(data))
        stop("`ndim` should be less than the number of columns.",
             call. = FALSE)

      nnmf_result <-
        NNLM::nnmf(
          A = data,
          k = pars$ndim,
          method = pars$method,
          loss = pars$loss,
          max.iter = pars$max.iter,
          n.threads = pars$n.threads,
          verbose = pars$verbose,
          rel.tol = pars$rel.tol
        )
      other.data <- list(H = nnmf_result$H, p = ncol(data))

      W <- nnmf_result$W
      colnames(W) <- paste0("NNMF", 1:ncol(W))

      # evaluate results here for functions

      appl <- function(x) {

        appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
        proj <- if (inherits(x, "dimRedData")) x@data else x

        if (!is.matrix(proj))
          proj <- as.matrix(proj)

        if (ncol(proj) != other.data$p)
          stop("x must have the same number of dimensions ",
               "as the original data (", other.data$p, ")",
               call. = FALSE)
        pars$x <- t(other.data$H)
        pars$y <- t(proj)
        pars$ndim <- NULL
        pars$verbose <- NULL
        proj <- do.call(NNLM::nnlm, pars)

        proj <- as.data.frame(t(proj$coefficients))
        names(proj) <- paste0("NNMF", 1:ncol(proj))

        proj <- new("dimRedData", data = proj, meta = appl.meta)
        return(proj)
      }

      inv <- function(x) {
        appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
        proj <- if (inherits(x, "dimRedData")) x@data else x
        if (ncol(proj) > ncol(data))
          stop("x must have less or equal number of dimensions ",
               "as the original data")

        reproj <- proj %*% other.data$H
        reproj <- new("dimRedData", data = reproj, meta = appl.meta)

        return(reproj)
      }

      res <- new(
        "dimRedResult",
        data         = new("dimRedData",
                           data = W,
                           meta = meta),
        org.data     = orgdata,
        apply        = appl,
        inverse      = inv,
        has.org.data = keep.org.data,
        has.apply    = TRUE,
        has.inverse  = TRUE,
        method       = "NNMF",
        pars         = pars,
        other.data   = other.data
      )

      return(res)
    })
)
