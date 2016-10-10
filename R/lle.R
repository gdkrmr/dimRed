#' Local Linear Embedding
#'
#' Instance of \code{\link{dimRedMethod}} for Local Linear Embedding.
#' 
#' For details see \code{\link[lle]{lle}}
#'
#' @examples
#' dat <- loadDataSet("3D S Curve")
#' lle <- LLE()
#' emb <- lle@fun(dat, lle@stdpars)
#'
#' 
#' plot(emb@data@data)
#' 
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @export
LLE <- setClass(
    'LLE',
    contains = 'dimRedMethod',
    prototype = list(
        stdpars = list(knn = 50, ndim = 2),
        fun = function (data, pars,
                        keep.org.data = TRUE) {
        chckpkg('lle')
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
)
