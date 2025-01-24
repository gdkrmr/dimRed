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
#'   \item{method}{character, which algorithm should be used. See
#'    \code{\link[NMF]{nmf}} for possible values. Defaults to
#'    "brunet"}
#'   \item{nrun}{integer, the number of times the computations are
#'    conducted. See \code{\link[NMF]{nmf}}}
#'   \item{seed}{integer, a value to control the random numbers used.}
#'   \item{options}{named list, other options to pass to  \code{\link[NMF]{nmf}}}
#' }
#'
#' @section Implementation:
#'
#' Wraps around \code{\link[NMF]{nmf}}. Note that the estimation uses random
#'  numbers. To create reproducible results, set the random number seed in the
#'  function call. Also, in many cases, the computations will be conducted
#'  in parallel using multiple cores. To disable this, use the option
#'  \code{.pbackend = NULL}.
#'
#' @references
#'
#' Lee, D.D., Seung, H.S., 1999. Learning the parts of objects by non-negative
#' matrix factorization. Nature 401, 788-791. https://doi.org/10.1038/44565
#'
#' @examples
#' if(requireNamespace(c("NNMF", "MASS"), quietly = TRUE)) {
#'
#' set.seed(4646)
#' dat <- loadDataSet("Iris")
#' emb <- embed(dat, "NNMF")
#'
#' plot(emb)
#'
#' # project new values:
#' nn_proj <- predict(emb, dat[1:7])
#' plot(nn_proj)
#'
#' }
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
                   method = "brunet",
                   nrun = 1,
                   seed = sample.int(10^5, 1),
                   options = list()),
    fun = function (data, pars, keep.org.data = TRUE) {
      chckpkg("NMF")
      chckpkg("MASS")
      ## TODO: remove this, depends on https://github.com/renozao/NMF/issues/114
      ## require("NMF")

      meta <- data@meta
      orgdata <- if (keep.org.data) data@data else matrix(0, 0, 0)
      data <- data@data
      if (!is.matrix(data))
        data <- as.matrix(data)
      # NMF expects variables in rows and samples in columns
      data <- t(data)
      if (pars$ndim > nrow(data))
        stop("`ndim` should be less than the number of columns.",
             call. = FALSE)
      if (length(pars$method) != 1)
        stop("only supply one `method`", call. = FALSE)

      args <- list(x = quote(data), rank = pars$ndim, method = pars$method,
                   nrun = pars$nrun, seed = pars$seed)

      if (length(pars$options) > 0)
        args <- c(args, pars$options)

      nmf_result <- do.call(NMF::nmf, args)

      # this should work but doesn't
      # call <- c(list(quote(NMF::nmf)), args)

      w <- NMF::basis(nmf_result)
      h <- t(NMF::coef(nmf_result))

      colnames(w) <- paste0("NNMF", 1:ncol(w))

      other.data <- list(w = w)
      colnames(h) <- paste0("NNMF", 1:ncol(h))

      # evaluate results here for functions

      appl <- function (x) {
        appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
        dat <- if (inherits(x, "dimRedData")) x@data else x

        if (!is.matrix(dat))
          dat <- as.matrix(dat)

        if (ncol(dat) != nrow(w))
          stop("x must have the same number of columns ",
               "as the original data (", nrow(w), ")",
               call. = FALSE)

        res <- dat %*% t(MASS::ginv(w))

        colnames(res) <- paste0("NNMF", 1:ncol(res))

        scores <- new("dimRedData", data = res, meta = appl.meta)
        return(scores)
      }

      inv <- function (x) {
        appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
        proj <- if (inherits(x, "dimRedData")) x@data else x

        if (ncol(proj) > ncol(w))
          stop("x must have less or equal number of dimensions ",
               "as the original data")

        res <- tcrossprod(proj, w)
        colnames(res) <- colnames(data)

        res <- new("dimRedData", data = res, meta = appl.meta)
        return(res)
      }

      ## inv <- function(x) {
      ##   appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
      ##   proj <- if (inherits(x, "dimRedData")) x@data else x
      ##   if (ncol(proj) > ncol(data))
      ##     stop("x must have less or equal number of dimensions ",
      ##          "as the original data")

      ##   reproj <- proj %*% other.data$H
      ##   reproj <- new("dimRedData", data = reproj, meta = appl.meta)

      ##   return(reproj)
      ## }

      res <- new(
        "dimRedResult",
        data         = new("dimRedData",
                           data = h,
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
    },
    requires = c("NMF", "MASS"))
)
