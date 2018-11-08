#' t-Distributed Stochastic Neighborhood Embedding
#'
#' An S4 Class for t-SNE.
#'
#' t-SNE is a method that uses Kullback-Leibler divergence between the
#' distance matrices in high and low-dimensional space to embed the
#' data. The method is very well suited to visualize complex
#' structures in low dimensions.
#'
#' @template dimRedMethodSlots
#'
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#' t-SNE can take the following parameters:
#' \describe{
#'   \item{d}{A distance function, defaults to euclidean distances}
#'   \item{perplexity}{The perplexity parameter, roughly equivalent to neighborhood size.}
#'   \item{theta}{Approximation for the nearest neighbour search, large values are more inaccurate.}
#'   \item{ndim}{The number of embedding dimensions.}
#' }
#'
#' @section Implementation:
#'
#' Wraps around \code{\link[Rtsne]{Rtsne}}, which is very well
#' documented. Setting \code{theta = 0} does a normal t-SNE, larger
#' values for \code{theta < 1} use the Barnes-Hut algorithm which
#' scales much nicer with data size. Larger values for perplexity take
#' larger neighborhoods into account.
#'
#' @references
#' Maaten, L. van der, 2014. Accelerating t-SNE using Tree-Based
#' Algorithms. Journal of Machine Learning Research 15, 3221-3245.
#'
#' van der Maaten, L., Hinton, G., 2008. Visualizing Data using
#' t-SNE. J. Mach. Learn. Res. 9, 2579-2605.
#'
#' @examples
#' \dontrun{
#' dat <- loadDataSet("3D S Curve", n = 300)
#'
#' ## using the S4 class directly:
#' tsne <- tSNE()
#' emb <- tsne@fun(dat, tsne@stdpars)
#'
#' ## using embed()
#' emb2 <- embed(dat, "tSNE", perplexity = 80)
#'
#' plot(emb, type = "2vars")
#' plot(emb2, type = "2vars")
#' }
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @export tSNE
#' @exportClass tSNE
tSNE <- setClass(
    "tSNE",
    contains = "dimRedMethod",
    prototype = list(
        stdpars = list(d = stats::dist,
                       perplexity = 30,
                       theta = 0.5,
                       ndim = 2),
        fun = function (data, pars,
                        keep.org.data = TRUE) {
        chckpkg("Rtsne")

        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        indata <- data@data

        outdata <- Rtsne::Rtsne(pars$d(indata),
                                perplexity = pars$perplexity,
                                theta = pars$theta,
                                dims = pars$ndim)$Y

        colnames(outdata) <- paste0("tSNE", 1:ncol(outdata))

        return(new(
            "dimRedResult",
            data         = new("dimRedData",
                               data = outdata,
                               meta = meta),
            org.data     = orgdata,
            has.org.data = keep.org.data,
            method       = "tsne",
            pars         = pars
        ))
    })
)
