#' Diffusion Maps
#'
#' Instance of \code{\link{dimRedMethod}} to construct a diffuson map. 
#' 
#' Wraps around \code{\link[diffusionMap]{diffuse}}, see there for
#' details.
#'
#' Parameters are a list with elements
#' \describe{
#'   \item{d}{a function transforming a matrix row wise into a distance matrix or \code{dist} object, e.g.\ \code{\link[stats]{dist}}.}
#'   \item{ndim}{The number of dimensions}
#'   \item{eps}{The epsilon parameter that determines the
#'      diffusion weight matrix from a distance matrix d,
#'      \eqn{exp{-d^2/eps}}, if set to \code{"auto"} it will
#'      be set to the median distance to the 0.01*n nearest
#'      neighbor.}
#'   \item{t}{Time-scale parameter. The recommended value, 0,
#'      uses multiscale geometry.}
#' }
#'
#' 
#' @examples
#' dat <- loadDataSet("3D S Curve")
#' emb <- diffmap@fun(dat, diffmap@stdpars)
#' 
#' plot(emb, type = "2vars")
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' 
#' @export
diffmap <- new('dimRedMethod',
               stdpars = list(d = stats::dist, ndim = 2, eps = "auto", t = 0),
               fun = function (data, pars,
                               keep.org.data = TRUE) {
    chckpkg('diffusionMap')

    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data

    distmat <- pars$d(indata)
    if(pars$eps == "auto") pars$eps <- diffusionMap::epsilonCompute(distmat)
    outdata <- diffusionMap::diffuse(
                                 D = distmat,
                                 t = pars$t,
                                 eps.val = pars$eps,
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
