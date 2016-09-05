#' implements Dimensionality Rediction via Regression for the use with dimRed
#'
#' uses \code{\link[DRR]{DRR}} internally. Has methods for forward and inverse calculations.
#'
#'
#' @param data an object of class \code{\link{dimRedData-class}}
#' @param pars a list with parameters
#'
#' @return an object of class \code{\link{dimRedResult-class}}
#'
#' @examples
#'
#' dat <- loadDataSet("3D S Curve", n = 500)
#' emb <- drr@fun(dat, pars = list(ndim = 2))
#'
#' plot(emb@data@data)
#'
#' 
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @import DRR
#' 
#' @export
drr <- new('dimRedMethod',
           stdpars = list(ndim            = 2,
                          lambda          = c(0, 10^(-3:2)),
                          kernel          = 'rbfdot',
                          kernel.pars     = list(sigma = 10^(-3:4)),
                          pca             = TRUE,
                          pca.center      = TRUE,
                          pca.scale       = FALSE,
                          fastcv          = FALSE,
                          cv.folds        = 5,
                          fastcv.test     = NULL,
                          fastkrr.nblocks = 4,
                          verbose         = TRUE),
           fun = function (data, pars,
                           keep.org.data = TRUE) {
    if(!requireNamespace('DRR')) stop('require package "DRR"')
    if(!requireNamespace("kernlab")) stop("require 'kernlab' package")


    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data

    res <- do.call(DRR::drr, c(list(X = indata), pars))

    outdata <- res$fitted.data
    colnames(outdata) <- paste0("DRR", 1:ncol(outdata))
    
    appl <- function(x){
        appl.meta <- if(inherits(x, 'dimRedData')) x@meta else data.frame()
        proj <- if(inherits(x, 'dimRedData')) x@data else x
        
        if(ncol(proj) != ncol(data@data))
            stop("x must have the same number of dimensions as the original data")

        appl.out <- new('dimRedData', data = res$apply(proj), meta = appl.meta)
        dimnames(appl.out@data) <- list(
            rownames(x), paste0("DRR", seq_len(ncol(appl.out@data)))
        )
        return(appl.out)
    }

    inv <- function(x) {
        appl.meta <- if(inherits(x, 'dimRedData')) x@meta else data.frame()
        proj <- if(inherits(x, 'dimRedData')) x@data else x
        
        if(ncol(proj) > ncol(data@data))
            stop("x must have less or equal number of dimensions as the original data")

        inv.out <- new('dimRedData', data = res$inverse(proj), meta = appl.meta)
        dimnames(inv.out@data) <- list(rownames(proj), colnames(data@data))
        return(inv.out)
    }
    
    
    return(
        new('dimRedResult',
            data = new('dimRedData',
                       data = outdata,
                       meta = meta),
            org.data = orgdata,
            apply = appl,
            inverse = inv,
            has.org.data = keep.org.data,
            has.apply = TRUE,
            has.inverse = TRUE,
            method = 'drr',
            pars = pars
            )
    )
})
