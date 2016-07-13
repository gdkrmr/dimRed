#' Soft ordinal embedding
#'
#' implements soft ordinal embedding
#'
#' for details see \code{\link[loe]{SOE}}
#'
#'
#' @examples
#' dat <- loadDataSet("3D S Curve", n = 50)
#' emb <- soe@fun(dat)
#'
#' 
#' plot(emb@data@data)
#'
#'
#' @include dimRed-class.R
#'
#' @export
soe <- new('dimRedMethod',
           fun = function (data,
                           pars = list(d = dist, knn = 50, ndim = 2),
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
        data = new('dimRedData',
                   data = outdata,
                   meta = meta),
        org.data = orgdata,
        has.org.data = keep.org.data
    ))
})
