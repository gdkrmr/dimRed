#' Laplacian Eigenmaps
#'
#' An S4 Class implementing Laplacian Eigenmaps
#'
#' Laplacian Eigenmaps use a kernel and were originally developed to
#' separate non-convex clusters.
#' 
#' @template dimRedMethodSlots
#' 
#' @template dimRedMethodGeneralUsage
#' 
#' @section Parameters:
#' \code{LaplacianEigenmaps} can take the following parameters:
#' \describe{
#'   \item{d}{a distance function to calculate the distance matrix}
#'   \item{knn}{The number of nearest neighbors to use for the knn graph.}
#'   \item{ndim}{the number of output dimensions.}
#' 
#'   \item{t}{Parameter for the transformation of the distance matrix
#'   by \eqn{w=exp(-d^2/t)}, larger values give less weight to
#'   differences in distance, \code{t == Inf} treats all distances != 0 equally.}
#'   \item{norm}{logical, should the normed laplacian be used?}
#' }
#'
#' @section Implementation:
#' Wraps around \code{\link[loe]{spec.emb}}.
#'
#' @references
#' Belkin, M., Niyogi, P., 2003. Laplacian Eigenmaps for
#' Dimensionality Reduction and Data Representation. Neural
#' Computation 15, 1373.
#'
#' @examples
#' dat <- loadDataSet("3D S Curve")
#' leim <- LaplacianEigenmaps()
#' emb <- leim@fun(dat, leim@stdpars)
#'
#' 
#' plot(emb@data@data)
#'
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @export LaplacianEigenmaps
#' @exportClass LaplacianEigenmaps
LaplacianEigenmaps <- setClass(
    "LaplacianEigenmaps",
    contains = "dimRedMethod",
    prototype = list(
        stdpars = list(d = stats::dist, knn = 50, ndim = 2,
                       t = Inf, norm = TRUE),
        fun = function (data, pars,
                        keep.org.data = TRUE) {
        chckpkg("loe")

        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        indata <- data@data

        if (is.null(pars$d))     pars$d    <- dist
        if (is.null(pars$knn))   pars$knn  <- 50
        if (is.null(pars$ndim))  pars$ndim <- 2
        if (is.null(pars$t))     pars$t    <- Inf
        if (is.null(pars$norm))  pars$norm <- TRUE
        
        if (is.infinite(pars$t)) {
            data.adj <- loe::make.kNNG(as.matrix(pars$d(indata)), pars$knn, symm = TRUE)
        } else {
            data.adj <- loe::make.kNNG(as.matrix(pars$d(indata)), pars$knn, symm = TRUE, weight = TRUE)
            data.inds <- data.adj != 0
            data.adj[data.inds] <- exp(-(data.adj[data.inds]^2)/pars$t) + 1e-10
        }
        outdata <- loe::spec.emb(data.adj, pars$ndim, pars$norm)
        if (is.null(dim(outdata))) {
            dim(outdata) <- c(length(outdata), 1)
        }

        
        colnames(outdata) <- paste0("LEIM", 1:ncol(outdata))

        return(new(
            "dimRedResult",
            data         = new("dimRedData",
                               data = outdata,
                               meta = meta),
            org.data     = orgdata,
            has.org.data = keep.org.data,
            method       = "leim",
            pars         = pars
        ))
    })
)
