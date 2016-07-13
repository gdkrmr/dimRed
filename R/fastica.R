


#' FastICA
#'
#' the fast ICA algoritm
#'
#' for details see \code{\link[fastICA]{fastICA}}
#'
#' @examples
#' dat <- loadDataSet("3D S Curve")
#' emb <- fastica@fun(dat, pars = list(ndim = 2))
#'
#' plot(emb@data@data)
#'
#'
#' @include dimRed-class.R
#' @export
fastica <- new('dimRedMethod',
               fun = function (data,
                               pars = list(ndim = 2),
                               keep.org.data = TRUE) {
    if(!requireNamespace('fastICA')) stop('require package fastICA')
                   
    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    orgdata.colmeans <- colMeans(orgdata)
    indata <- data@data

    res <- fastICA::fastICA(indata, n.comp = pars$ndim, method = 'C')

    outdata <- res$S
    colnames(outdata) <- paste0("ICA", 1:ncol(outdata))

    appl <- function(x)
        scale(x, center = orgdata.colmeans, scale = FALSE)%*%res$K%*%res$W
    inv <- function(x)
        scale(x%*%res$A, center = -orgdata.colmeans, scale = FALSE)
    
    return(new(
        'dimRedResult',
        data = new('dimRedData',
                   data = outdata,
                   meta = meta),
        org.data = orgdata,
        has.org.data = keep.org.data,
        apply = appl,
        inverse = inv,
        has.apply = TRUE,
        has.inverse = TRUE
    ))
})
