

#' Metric dimensional scaling
#'
#' Fit a metric dimensional scaling
#'
#' for details see \code{\link[stats]{cmdscale}}
#'
#' @examples
#' dat <- loadDataSet("3D S Curve")
#' emb <- mds@fun(dat)
#'
#' plot(emb@data@data)
#'
#'
#' 
#' @include dimRed-class.R
#' @export
mds <- new('dimRedMethod',
           fun = function (data,
                           pars = list(d = dist, ndim = 2),
                           keep.org.data = TRUE) {

    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data

    outdata <- stats::cmdscale(pars$d(indata), k = pars$ndim)

    colnames(outdata) <- paste0("MDS", 1:ncol(outdata))

    return(new(
        'dimRedResult',
        data         = new('dimRedData',
                           data = outdata,
                           meta = meta),
        org.data     = orgdata,
        has.org.data = keep.org.data,
        method       = "mds",
        pars         = pars
    ))
})
