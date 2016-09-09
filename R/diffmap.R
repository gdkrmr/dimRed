#' Diffusion Maps
#'
#' Instance of \code{\link{dimRedMethod}} to construct a diffuson maps.
#' 
#' For details see \code{\link[diffusionMap]{diffuse}}.
#'
#' 
#' @examples
#' dat <- loadDataSet("3D S Curve")
#' emb <- diffmap@fun(dat, diffmap@stdpars)
#' 
#' plot(emb@data@data)
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' 
#' @export
diffmap <- new('dimRedMethod',
               stdpars = list(d = stats::dist, ndim = 2, t = 0),
               fun = function (data, pars,
                               keep.org.data = TRUE) {
    # if(!requireNamespace('diffusionMap')) stop('require the diffusionMap package')

    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data


    outdata <- diffusionMap::diffuse(
                                 D = pars$d(indata),
                                 t = pars$t,
                                 neigen = pars$ndim,
                                 maxdim = pars$ndim
                             )$X
    
    colnames(outdata) <- paste0("diffMap", 1:ncol(outdata))

    return(new(
        'dimRedResult',
        data = new('dimRedData',
                   data = outdata,
                   meta = meta),
        org.data = orgdata,
        has.org.data = keep.org.data,
        method       = "diffmap",
        pars         = pars
    ))
})
