#' Principal Component Analysis 
#'
#' Instance of \code{\link{dimRedMethod}} for Principal Component Analysis.
#' 
#' For details see \code{\link{prcomp}}.
#'
#' @examples
#' dat <- loadDataSet("3D S Curve")
#' emb <- pca@fun(dat, pca@stdpars)
#'
#' plot(emb, type = "2vars")
#' plot(emb@inverse(emb@data), type = "3vars")
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' 
#' @export
pca <- new('dimRedMethod',
           stdpars = list(ndim = 2,
                          center = TRUE,
                          scale. = FALSE),
           fun = function (data, pars,
                           keep.org.data = TRUE) {
    ndim <- pars$ndim
    pars$ndim <- NULL
    
    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    data <- data@data
    res <- do.call(
        prcomp,
        c(list(x = data), pars)
    )

    # evaluate results here for functions
    data <- res$x[, seq_len(ndim), drop = FALSE]
    ce <- res$center
    sc <- res$scale
    rot <- res$rotation[,seq_len(ndim)]
    rerot <- t(rot)

    
    appl <- function(x) {
        appl.meta <- if(inherits(x, 'dimRedData')) x@meta else data.frame()
        proj <- if(inherits(x, 'dimRedData')) x@data else x
        
        if(ncol(proj) != ncol(orgdata))
            stop("x must have the same number of dimensions as the original data")

        
        if(ce[1]  != FALSE) proj <- t(apply(proj, 1, function(x) x - ce))
        if(sc[1]  != FALSE) proj <- t(apply(proj, 1, function(x) x / sc))
        proj <- proj %*% rot

        proj <- new('dimRedData', data = proj, meta = appl.meta)
        return(proj)
    }
    inv  <- function(x) {
        appl.meta <- if(inherits(x, 'dimRedData')) x@meta else data.frame()
        proj <- if(inherits(x, 'dimRedData')) x@data else x
        if(ncol(proj) > ncol(data))
            stop("x must have less or equal number of dimensions as the original data")


        d <- ncol(proj)
        reproj <- proj %*% rerot[seq_len(d),]

        if(sc[1] != FALSE) reproj <- t(apply(reproj, 1, function(x) x * sc))
        if(ce[1] != FALSE) reproj <- t(apply(reproj, 1, function(x) x + ce))

        reproj <- new('dimRedData', data = reproj, meta = appl.meta)
        
        return(reproj)
    }

    res <- new(
        'dimRedResult',
        data         = new('dimRedData',
                           data = data,
                           meta = meta),
        org.data     = orgdata,
        apply        = appl,
        inverse      = inv,
        has.org.data = keep.org.data,
        has.apply    = TRUE,
        has.inverse  = TRUE,
        method       = "pca",
        pars         = pars
    )
    
    return(res)
})
