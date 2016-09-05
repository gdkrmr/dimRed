
#' Non-metric dimensional scaling
#'
#' Implements non metric dimensional scaling
#'
#' for details see \code{\link[vegan]{monoMDS}}
#'
#' @examples
#' dat <- loadDataSet("3D S Curve")
#' emb <- nmds@fun(dat)
#'
#' plot(emb@data@data)
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' 
#' @export
nmds <- new('dimRedMethod',
            stdpars = list(d = dist, ndim = 2),
            fun = function (data, pars,
                            keep.org.data = TRUE) {
    if(!requireNamespace('vegan')) stop('require the vegan package')

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
