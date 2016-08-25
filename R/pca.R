

#' implements PCA for the use with dimRed
#'
#' uses the \code{\link{prcomp}} function internally. Has methods for
#' forward and inverse calculations.
#'
#' @param data an object of class \code{\link{dimRedData-class}}.
#' @param pars an object of class \code{\link{dimRedMethodPars-class}}
#'

#' @return an object of class \code{\link{dimRedResult-class}}
#' 
#'
#' dat <- loadDataSet("3D S Curve")
#' emb <- pca@fun(dat)
#'
#' plot(emb@data@data)
#'
#' @include dimRed-class.R
#' @export
pca <- new('dimRedMethod', fun = function (data, pars = list(ndim = 2,
                                                             center = FALSE,
                                                             scale. = FALSE),
                                           keep.org.data = TRUE) {
    if(is.null(pars$ndim))
        pars$ndim <- ncol(data@data)
    
    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    data <- data@data
    res <- do.call(
        prcomp,
        c(list(x = data), pars)
    )

    # evaluate results here for functions
    data <- res$x[,1:pars$ndim,drop = FALSE]
    ce <- res$center
    sc <- res$scale
    rot <- res$rotation
    rerot <- t(res$rot)

    
    appl <- function(x) {
        appl.meta <- if(inherits(x, 'dimRedData'))
                         x@meta
                     else
                         matrix(numeric(0), 0,0)
        proj <- if(inherits(x, 'dimRedData'))
                    x@data
                else
                    x
        if(ncol(proj) != ncol(data))
            stop("x must have the same number of dimensions as the original data")

        
        if(ce[1]  != FALSE) proj <- t(apply(proj, 1, function(x) x - ce))
        if(sc[1]  != FALSE) proj <- t(apply(proj, 1, function(x) x / sc))
        proj <- proj %*% rot

        proj <- new('dimRedData', data = proj, meta = appl.meta)
        return(proj)
    }
    inv  <- function(x) {
        appl.meta <- if(inherits(x, 'dimRedData'))
                         x@meta
                     else 
                         matrix(numeric(0), 0,0)
        proj <- if(inherits(x, 'dimRedData'))
                    x@data
                else
                    x
        if(ncol(proj) > ncol(data))
            stop("x must have less or equal number of dimensions as the original data")


        d <- ncol(proj)
        reproj <- proj %*% rerot[1:d,]

        if(sc[1] != FALSE) reproj <- t(apply(reproj, 1, function(x) x * sc))
        if(ce[1] != FALSE) reproj <- t(apply(reproj, 1, function(x) x + ce))

        reproj <- new('dimRedData', data = reproj, meta = appl.meta)
        
        return(reproj)
    }

    res <- new(
        'dimRedResult',
        data = new('dimRedData',
                   data = data,
                   meta = meta),
        org.data = orgdata,
        apply = appl,
        inverse = inv,
        has.org.data = keep.org.data,
        has.apply = TRUE,
        has.inverse = TRUE
    )
    
    return(res)
})
