


#' Create Diffusion Maps
#'
#' Constructs a diffuson map
#'
#'
#' for details see \code{\link[diffusionMap]{diffuse}}
#'
#'
#' 
#' 
#' @examples
#' dat <- loadDataSet("3D S Curve")
#' emb <- diffmap@fun(dat)
#' 
#' plot(emb@data@data)
#'
#' @include dimRed-class.R
#' @export
diffmap <- new('dimRedMethod',
               fun = function (data,
                               pars = list(d = dist, knn = 50, ndim = 2,
                                           t = Inf, norm = TRUE),
                               keep.org.data = TRUE) {
    if(!requireNamespace('diffusionMap')) stop('require the diffusionMap package')

    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data


    outdata <- diffusionMap::diffuse(pars$d(indata), neigen = pars$ndim, maxdim = pars$ndim)$X
    
    colnames(outdata) <- paste0("diffMap", 1:ncol(outdata))

    return(new(
        'dimRedResult',
        data = new('dimRedData',
                   data = outdata,
                   meta = meta),
        org.data = orgdata,
        has.org.data = keep.org.data
    ))
})
