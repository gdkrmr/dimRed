



#' implements Dimensionality Rediction via Regression for the use with dimRed
#'
#' uses \code{\link[DRR]{DRR}} internally. Has methods for forward and inverse calculations.
#'
#'
#' @param data an object of class \code{\link{dimRedData-class}}
#' @param pars an object of class \code{\link{dimRedMethodPars-class}}
#'
#' @return an object of class \code{\link{dimRedResult-class}}
#'
#' @examples
#'
#' dat <- loadDataSet("3D S Curve")
#' emb <- drr@fun(dat, pars = list(ndim = 2))
#'
#' plot(emb@data@data)
#'
#' 
#' @include dimRed-class.R
#' @export
drr <- new('dimRedMethod',
           fun = function (data, pars = list(),
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
        appl.meta <- if(inherits(x, 'dimRedData'))
                         x@meta
                     else
                         matrix(numeric(0), 0,0)
        proj <- if(inherits(x, 'dimRedData'))
                    x@data
                else
                    x
        if(ncol(proj) != ncol(data@data))
            stop("x must have the same number of dimensions as the original data")

        return(new('dimRedData', data = res$apply(proj), meta = appl.meta))
    }

    inv <- function(x) {
        appl.meta <- if(inherits(x, 'dimRedData'))
                         x@meta
                     else 
                         matrix(numeric(0), 0,0)
        proj <- if(inherits(x, 'dimRedData'))
                    x@data
                else
                    x
        if(ncol(proj) > ncol(data@data))
            stop("x must have less or equal number of dimensions as the original data")

        return(new('dimRedData', data = res$inverse(proj), meta = appl.meta))
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
            has.inverse = TRUE
        )
    )
})
