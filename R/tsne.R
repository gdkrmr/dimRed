#' t-Distributed Stochastic Neighborhood Embedding
#'
#' Instance of \code{\link{dimRedMethod}} for t-Distributed Stochastic Neighborhood Embedding.
#' 
#' For details see \code{\link[Rtsne]{Rtsne}}
#'
#' @examples
#' dat <- loadDataSet("3D S Curve")
#' emb <- tsne@fun(dat, tsne@stdpars)
#'
#' 
#' plot(emb@data@data)
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @export
tsne <- new('dimRedMethod',
            stdpars = list(d = stats::dist,
                           perplexity = 30,
                           theta = 0.5,
                           ndim = 2),
            fun = function (data, pars,
                            keep.org.data = TRUE) {
    if(!requireNamespace('Rtsne')) stop('require the Rtsne package')

    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data

    outdata <- Rtsne::Rtsne(pars$d(indata),
                            perplexity = pars$perplexity,
                            theta = pars$theta,
                            ndim = pars$ndim)$Y

    colnames(outdata) <- paste0("tSNE", 1:ncol(outdata))

    return(new(
        'dimRedResult',
        data         = new('dimRedData',
                           data = outdata,
                           meta = meta),
        org.data     = orgdata,
        has.org.data = keep.org.data,
        method       = "tsne",
        pars         = pars
    ))
})
