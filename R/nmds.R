#' Non-Metric Dimensional Scaling
#'
#' An S4 Class implementing Non-Metric Dimensional Scaling.
#'
#' A non-linear extension of MDS using monotonic regression
#' 
#' @template dimRedMethodSlots
#' 
#' @template dimRedMethodGeneralUsage
#' 
#' @section Parameters:
#' nMDS can take the following parameters:
#' \describe{
#'   \item{d}{A distance function.}
#'   \item{ndim}{The number of embedding dimensions.}
#' }
#'
#' @section Implementation:
#' Wraps around the
#' \code{\link[vegan]{monoMDS}}. For parameters that are not
#' available here, the standard configuration is used.
#'
#' @examples
#' dat <- loadDataSet("3D S Curve", n = 1000)
#'
#' ## using the S4 classes:
#' nmds <- nMDS()
#' emb <- nmds@fun(dat, nmds@stdpars)
#'
#'
#' ## using embed()
#' emb2 <- embed(dat, "nMDS", d = function(x) exp(dist(x)))
#'
#' 
#' plot(emb, type = '2vars')
#' plot(emb2, type = '2vars')
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @export nMDS
#' @exportClass nMDS
nMDS <- setClass(
    'nMDS',
    contains = 'dimRedMethod',
    prototype = list(
        stdpars = list(d = stats::dist, ndim = 2),
        fun = function (data, pars,
                        keep.org.data = TRUE) {
        chckpkg('vegan')

        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        indata <- data@data

        outdata <- vegan::monoMDS(pars$d(indata), k = pars$ndim)$points

        colnames(outdata) <- paste0("NMDS", 1:ncol(outdata))

        return(new(
            'dimRedResult',
            data         = new('dimRedData',
                               data = outdata,
                               meta = meta),
            org.data     = orgdata,
            has.org.data = keep.org.data,
            method       = "nmds",
            pars         = pars
        ))
    })
)
