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
#' dat <- loadDataSet("Iris")
#'
#' set.seed(4646)
#' factorization <- embed(dat, "NNMF")
#'
#' proj_dat <- factorization@apply(dat)
#'
#' plot(proj_dat@data[, 1], proj_dat@data[, 2])
#'
#' # project new values:
#' 
#' nn_proj <- predict(factorization, iris[1:7, 1:4])
#' nn_proj
#' 
#' inverse(factorization, nn_proj)
#' 
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @importFrom NMF nmf
#' @export NNMF
#' @exportClass NNMF
NNMF <- setClass(
  "NNMF",
  contains = "dimRedMethod",
  prototype = list(
    stdpars = list(ndim = 2L,
                   method = "brunet",
                   nrun = 10,
                   seed = sample.int(10^5, 1),
                   options = list()),
    fun = function (data, pars, keep.org.data = TRUE) {
      chckpkg("NMF")

      meta <- data@meta
      orgdata <- if (keep.org.data) data@data else NULL
      data <- data@data
      if (!is.matrix(data))
        data <- as.matrix(data)
      # NMF expects variables in rows and samples in columns ¯\_(ツ)_/¯
      data <- t(data)
      if (pars$ndim > nrow(data))
        stop("`ndim` should be less than the number of columns.",
             call. = FALSE)
      
      args <- list(x = quote(data), rank = pars$ndim, method = pars$method, 
                   nrun = pars$nrun, seed = pars$seed)
      

      if(length(pars$options) > 0) 
        args <- c(args, pars$options)

      nmf_result <- do.call(NMF::nmf, args)
      
      # this should work but doesn't 
      # call <- c(list(quote(NMF::nmf)), args)

      proj <- t(basis(nmf_result))
      other.data <- list(p = nrow(data), proj = proj)
      scores <- t(data) %*% t(proj)
      scores <- as.data.frame(scores)
      names(scores) <- paste0("NNMF", 1:ncol(scores))
      
      # evaluate results here for functions

      appl <- function(x) {
        appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
        dat <- if (inherits(x, "dimRedData")) x@data else x

        if (!is.matrix(dat))
          dat <- as.matrix(dat)

        if (ncol(dat) != other.data$p)
          stop("x must have the same number of columns ",
               "as the original data (", other.data$p, ")",
               call. = FALSE)

        scores <- as.data.frame(dat %*% t(other.data$proj))
        names(scores) <- paste0("NNMF", 1:ncol(scores))

        scores <- new("dimRedData", data = scores, meta = appl.meta)
        return(scores)
      }

      inv <- function(x) {
        appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
        dat <- if (inherits(x, "dimRedData")) x@data else x
        if (!is.matrix(dat))
          dat <- as.matrix(dat)

        scores <- dat %*% other.data$proj
        scores <- new("dimRedData", data = scores, meta = appl.meta)

        return(scores)
      }

      res <- new(
        "dimRedResult",
        data         = new("dimRedData",
                           data = scores,
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
