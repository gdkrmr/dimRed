#' Umap embedding
#'
#' An S4 Class implementing the UMAP algorithm
#'
#' Uniform Manifold Approximation is a gradient descend based algorithm that
#' gives results similar to t-SNE, but scales better with the number of points.
#'
#' @template dimRedMethodSlots
#'
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#'
#' UMAP can take the follwing parameters:
#' \describe{
#'   \item{ndim}{The number of embedding dimensions.}
#'   \item{knn}{The number of neighbors to be used.}
#'   \item{d}{The distance metric to use.}
#'   \item{method}{\code{"naive"} for an R implementation, \code{"python"}
#'     for the reference implementation.}
#' }
#'
#' Other method parameters can also be passed, see
#' \code{\link[umap]{umap.defaults}} for details. The ones above have been
#' standardized for the use with \code{dimRed} and will get automatically
#' translated for \code{\link[umap]{umap}}.
#'
#' @section Implementation:
#'
#' The dimRed package wraps the \code{\link[umap]{umap}} packages which provides
#' an implementation in pure R and also a wrapper around the original python
#' package \code{umap-learn} (https://github.com/lmcinnes/umap/)
#'
#' The \code{"naive"} implementation is a pure R implementation and considered
#' experimental at the point of writing this, it is also much slower than the
#' python implementation.
#'
#' The \code{"python"} implementation is the reference implementation used by
#' McInees et. al. (2018). It requires the \code{\link[reticulate]{reticulate}}
#' package for the interaction with python and the python package
#' \code{umap-learn} installed (use \code{pip install umap-learn}).
#'
#' @references
#'
#' McInnes, Leland, and John Healy.
#' "UMAP: Uniform Manifold Approximation and Projection for Dimension Reduction."
#' https://arxiv.org/abs/1802.03426
#'
#' @examples
#' \dontrun{
#' dat <- loadDataSet("3D S Curve", n = 300)
#' emb <- embed(dat, "UMAP", .mute = NULL, knn = 10)
#' plot(emb, type = "2vars")
#' }
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @export UMAP
#' @exportClass UMAP
UMAP <- setClass(
  "UMAP",
  contains = "dimRedMethod",
  prototype = list(
    stdpars = list(
      knn = 15,
      ndim = 2,
      d = "euclidean",
      method = "umap-learn"
    ),
    fun = function (data, pars,
                    keep.org.data = TRUE) {
      chckpkg("umap")
      if (pars$method == "python") {
        chckpkg("reticulate")
        if (!reticulate::py_module_available("umap"))
          stop("cannot find python umap, install with `pip install umap-learn`")
      }

      meta <- data@meta
      orgdata <- if (keep.org.data) data@data else NULL
      indata <- data@data

      ## Create config
      umap_call_pars <- umap::umap.defaults
      umap_call_pars$n_neighbors  <- pars$knn
      umap_call_pars$n_components <- pars$ndim
      umap_call_pars$metric       <- pars$d
      umap_call_pars$method <- pars$method
      umap_call_pars$d      <- indata

      pars_2 <- pars
      pars_2$knn <- NULL
      pars_2$ndim <- NULL
      pars_2$d <- NULL
      pars_2$method <- NULL

      for (n in names(pars_2))
        umap_call_pars[[n]] <- pars_2[[n]]

      ## Do the embedding
      outdata <- do.call(umap::umap, umap_call_pars)

      ## Post processing
      colnames(outdata$layout) <- paste0("UMAP", 1:ncol(outdata$layout))

      appl <- function(x) {
        appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
        proj <- if (inherits(x, "dimRedData")) x@data else x

        if (ncol(proj) != ncol(orgdata))
          stop("x must have the same number of dimensions ",
               "as the original data")

        new_proj <- umap:::predict.umap(outdata, proj)

        colnames(new_proj) <- paste0("UMAP", 1:ncol(new_proj))
        rownames(new_proj) <- NULL

        out_data <- new("dimRedData", data = new_proj, meta = appl.meta)
        return(out_data)
      }

      return(new(
        "dimRedResult",
        data = new("dimRedData",
                   data = outdata$layout,
                   meta = meta),
        org.data = orgdata,
        apply        = appl,
        has.org.data = keep.org.data,
        has.apply    = TRUE,
        method = "UMAP",
        pars = pars
      ))
    }
  )
)
