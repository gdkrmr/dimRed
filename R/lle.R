#' Local linear embedding
#'
#' Fit a local linear embedding
#'
#' for details see \code{\link[lle]{lle}}
#'
#' @examples
#' dat <- loadDataSet("3D S Curve")
#' emb <- lle@fun(dat)
#'
#' 
#' plot(emb@data@data)
#' @include dimRed-class.R
#' @export
lle <- new('dimRedMethod',
           fun = function (data,
                           pars = list(knn = 50, ndim = 2),
                           keep.org.data = TRUE) {
    if(!requireNamespace('lle')) stop('require the lle package')
    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data

    outdata <- lle::lle(indata,
                        k = pars$knn,
                        m = pars$ndim)$Y
    if(is.null(dim(outdata))) {
        dim(outdata) <- c(length(outdata), 1)
    }
    colnames(outdata) <- paste0("LLE", 1:ncol(outdata))
 
    return(new(
        'dimRedResult',
        data         = new('dimRedData',
                           data = outdata,
                           meta = meta),
        org.data     = orgdata,
        has.org.data = keep.org.data,
        method       = "lle",
        pars         = pars
    ))
})
