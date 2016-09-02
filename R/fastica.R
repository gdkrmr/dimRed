


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
#' 
#' @export
fastica <- new('dimRedMethod',
               stdpars = list(ndim = 2),
               fun = function (data,
                               pars,
                               keep.org.data = TRUE) {
    if(!requireNamespace('fastICA')) stop('require package fastICA')
                   
    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    orgdata.colmeans <- colMeans(orgdata)
    indata <- data@data

    res <- fastICA::fastICA(indata, n.comp = pars$ndim, method = 'C')

    outdata <- res$S
    colnames(outdata) <- paste0("ICA", 1:ncol(outdata))

    appl <- function(x){
        appl.meta <- if(inherits(x, 'dimRedData'))
                         x@meta
                     else   
                         matrix(numeric(0), 0,0)
        
        proj <- if(inherits(x, 'dimRedData'))
                    x@data
                else 
                    x
        
        out <- scale(proj, center = orgdata.colmeans, scale = FALSE)%*%res$K%*%res$W
        return(new('dimRedData', data = out, meta = appl.meta))
    }

    inv <- function(x){
        appl.meta <- if(inherits(x, 'dimRedData'))
                         x@meta
                     else  
                         matrix(numeric(0), 0,0)
        
        proj <- if(inherits(x, 'dimRedData'))
                    x@data
                else
                    x
        
        out <- scale(proj%*%res$A[1:ncol(proj),], center = -orgdata.colmeans, scale = FALSE)
        reproj <- new('dimRedData', data = out, meta = appl.meta)
        return(reproj)
    }
    
    
    return(new(
        'dimRedResult',
        data         = new('dimRedData',
                           data = outdata,
                           meta = meta),
        org.data     = orgdata,
        has.org.data = keep.org.data,
        apply        = appl,
        inverse      = inv,
        has.apply    = TRUE,
        has.inverse  = TRUE,
        method       = "fastica",
        pars         = pars
    ))
})
