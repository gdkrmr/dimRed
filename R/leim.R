#' laplacian eigenmaps
#'
#' Fit laplacian eigenmaps
#'
#' for details see \code{\link[loe]{spec.emb}}
#'
#' @examples
#' dat <- loadDataSet("3D S Curve")
#' emb <- leim@fun(dat)
#'
#' 
#' plot(emb@data@data)
#'
#'
#' @include dimRed-class.R
#' 
#' @export
leim <- new('dimRedMethod',
           fun = function (data,
                           pars = list(d = dist, knn = 50, ndim = 2,
                                       t = Inf, norm = TRUE),
                           keep.org.data = TRUE) {
    if(!requireNamespace('loe')) stop('require the loe package')

    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data

    if(is.null(pars$d))     pars$d    <- dist
    if(is.null(pars$knn))   pars$knn  <- 50
    if(is.null(pars$ndim))  pars$ndim <- 2
    if(is.null(pars$t))     pars$t    <- Inf
    if(is.null(pars$norm))  pars$norm <- TRUE
        
    if(is.infinite(pars$t)) {
        data.adj <- loe::make.kNNG(as.matrix(pars$d(indata)), pars$knn, symm = TRUE)
    } else {
        data.adj <- loe::make.kNNG(as.matrix(pars$d(indata)), pars$knn, symm = TRUE, weight = TRUE)
        data.inds <- data.adj != 0
        data.adj[data.inds] <- exp(-(data.adj[data.inds]^2)/pars$t) + 1e-10
    }
    outdata <- loe::spec.emb(data.adj, pars$ndim, pars$norm)
    if(is.null(dim(outdata))) {
        dim(outdata) <- c(length(outdata), 1)
    }

    
    colnames(outdata) <- paste0("LEIM", 1:ncol(outdata))

    return(new(
        'dimRedResult',
        data         = new('dimRedData',
                           data = outdata,
                           meta = meta),
        org.data     = orgdata,
        has.org.data = keep.org.data,
        method       = "leim",
        pars         = pars
    ))
})
