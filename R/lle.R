## #' Locally Linear Embedding
## #'
## #' An S4 Class implementing Locally Linear Embedding (LLE)
## #'
## #' LLE approximates the points in the manifold by linear combination
## #' of its neighbors. These linear combinations are the same inside the
## #' manifold and in highdimensional space.
## #'
## #' @template dimRedMethodSlots
## #'
## #' @template dimRedMethodGeneralUsage
## #'
## #' @section Parameters:
## #' LLE can take the following parameters:
## #' \describe{
## #'   \item{knn}{the number of neighbors for the knn graph., defaults to 50.}
## #'   \item{ndim}{the number of embedding dimensions, defaults to 2.}
## #' }
## #'
## #' @section Implementation:
## #' Wraps around \code{\link[lle]{lle}}, only
## #' exposes the parameters \code{k} and \code{m}.
## #'
## #' @references
## #'
## #' Roweis, S.T., Saul, L.K., 2000. Nonlinear Dimensionality Reduction
## #' by Locally Linear Embedding. Science 290,
## #' 2323-2326. doi:10.1126/science.290.5500.2323
## #'
## #' @examples
## #' \dontrun{
## #' dat <- loadDataSet("3D S Curve", n = 500)
## #' emb <- embed(dat, "LLE", knn = 45)
## #' plot(emb, type = "2vars")
## #' }
## #' @include dimRedResult-class.R
## #' @include dimRedMethod-class.R
## #' @family dimensionality reduction methods
## #' @export LLE
## #' @exportClass LLE
## LLE <- setClass(
##     "LLE",
##     contains = "dimRedMethod",
##     prototype = list(
##         stdpars = list(knn = 50, ndim = 2),
##         fun = function (data, pars,
##                         keep.org.data = TRUE) {
##           chckpkg("lle")
##           meta <- data@meta
##           orgdata <- if (keep.org.data) data@data else NULL
##           indata <- data@data

##           outdata <- lle::lle(indata,
##                               k = pars$knn,
##                               m = pars$ndim)$Y
##           if (is.null(dim(outdata))) {
##             dim(outdata) <- c(length(outdata), 1)
##           }
##           colnames(outdata) <- paste0("LLE", 1:ncol(outdata))

##           return(new(
##             "dimRedResult",
##             data         = new("dimRedData",
##                                data = outdata,
##                                meta = meta),
##             org.data     = orgdata,
##             has.org.data = keep.org.data,
##             method       = "lle",
##             pars         = pars
##           ))
##         },
##         requires = c("lle")
##     )
## )
