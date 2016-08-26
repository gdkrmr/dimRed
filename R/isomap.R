

#' Isomap embedding
#'
#'
#' Fit the isomap embedding.
#'
#'
#' For details see \code{\link[vegan]{isomap}}
#'
#'
#' 
#' @examples
#' dat <- loadDataSet("3D S Curve")
#' emb <- isomap@fun(dat)
#'
#' plot(emb@data@data)
#' 
#' @include dimRed-class.R
#' 
#' @export
isomap <- new('dimRedMethod',
              fun = function (data,
                              pars = list(knn = 50,
                                          d = dist,
                                          ndim = 2),
                              keep.org.data = TRUE) {
    if(!requireNamespace('vegan')) stop('require the vegan package')

    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data

    outdata <- vegan::isomap(pars$d(indata),
                             ndim = pars$ndim,
                             k = pars$knn)$points

    colnames(outdata) <- paste0("Iso", 1:ncol(outdata))

    return(new(
        'dimRedResult',
        data         = new('dimRedData',
                           data = outdata,
                           meta = meta),
        org.data     = orgdata,
        has.org.data = keep.org.data,
        method       = "isomap",
        pars         = pars
    ))
})
