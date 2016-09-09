#' Soft Ordinal Embedding
#'
#' Instance of \code{\link{dimRedMethod}} for Soft Ordinal Embedding.
#' 
#' For details see \code{\link[loe]{SOE}}.
#'
#'
#' @examples
#' dat <- loadDataSet("3D S Curve", n = 50)
#' emb <- soe@fun(dat, soe@stdpars)
#'
#' 
#' plot(emb@data@data)
#'
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @import loe
#' @export
soe <- new('dimRedMethod',
           stdpars = list(d = dist, knn = 50, ndim = 2),
           fun = function (data,
                           pars,
                           keep.org.data = TRUE) {
    if(!requireNamespace('loe')) stop('require the loe package')

    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data
    
    outdata <- loe::SOE(loe::get.order(as.matrix(pars$d(indata))),
                        N = nrow(indata), p = pars$ndim)$X

    colnames(outdata) <- paste0("SOE", 1:ncol(outdata))

    return(new(
        'dimRedResult',
        data         = new('dimRedData',
                           data = outdata,
                           meta = meta),
        org.data     = orgdata,
        has.org.data = keep.org.data,
        method       = "soe",
        pars         = pars
    ))
})
