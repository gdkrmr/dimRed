#' Diffusion Maps
#'
#' An S4 Class implementing Diffusion Maps
#'
#' Diffusion Maps uses a diffusion probability matrix to robustly
#' approximate a manifold.
#'
#'
#' @template dimRedMethodSlots
#'
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#' Diffusion Maps can take the following parameters:
#' \describe{
#'   \item{d}{a function transforming a matrix row wise into a
#'     distance matrix or \code{dist} object,
#'     e.g. \code{\link[stats]{dist}}.}
#'   \item{ndim}{The number of dimensions}
#'   \item{eps}{The epsilon parameter that determines the
#'      diffusion weight matrix from a distance matrix \code{d},
#'      \eqn{exp(-d^2/eps)}, if set to \code{"auto"} it will
#'      be set to the median distance to the 0.01*n nearest
#'      neighbor.}
#'   \item{t}{Time-scale parameter. The recommended value, 0,
#'      uses multiscale geometry.}
#'   \item{delta}{Sparsity cut-off for the symmetric graph Laplacian,
#'     a higher value results in more sparsity and faster calculation.
#'     The predefined value is 10^-5.}
#' }
#'
#' @section Implementation:
#' Wraps around \code{\link[diffusionMap]{diffuse}}, see there for
#' details. It uses the notation of Richards et al. (2009) which is
#' slightly different from the one in the original paper (Coifman and
#' Lafon, 2006) and there is no \eqn{\alpha} parameter.
#' There is also an out-of-sample extension, see examples.
#'
#'
#' @references
#' Richards, J.W., Freeman, P.E., Lee, A.B., Schafer,
#'     C.M., 2009. Exploiting Low-Dimensional Structure in
#'     Astronomical Spectra. ApJ 691,
#'     32. doi:10.1088/0004-637X/691/1/32
#'
#' Coifman, R.R., Lafon, S., 2006. Diffusion maps. Applied and
#'     Computational Harmonic Analysis 21,
#'     5-30. doi:10.1016/j.acha.2006.04.006
#'
#' @examples
#' dat <- loadDataSet("3D S Curve", n = 500)
#'
#' ## use the S4 Class directly:
#' diffmap <- DiffusionMaps()
#' emb <- diffmap@fun(dat, diffmap@stdpars)
#'
#' ## simpler, use embed():
#' emb2 <- embed(dat, "DiffusionMaps")
#'
#' plot(emb, type = "2vars")
#'
#' samp <- sample(floor(nrow(dat) / 10))
#' embsamp <- diffmap@fun(dat[samp], diffmap@stdpars)
#' embother <- embsamp@apply(dat[-samp])
#' plot(embsamp, type = "2vars")
#' points(embother@data)
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @export DiffusionMaps
#' @exportClass DiffusionMaps
DiffusionMaps <- setClass(
    "DiffusionMaps",
    contains  = "dimRedMethod",
    prototype = list(
        stdpars = list(d     = stats::dist,
                       ndim  = 2,
                       eps   = "auto",
                       t     = 0,
                       delta = 1e-5),
        fun     = function (data, pars,
                            keep.org.data = TRUE) {
        chckpkg("diffusionMap")

        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        indata <- data@data

        distmat <- pars$d(indata)
        if (pars$eps == "auto")
            pars$eps <- diffusionMap::epsilonCompute(distmat)
        diffres <- diffusionMap::diffuse(
                                     D = distmat,
                                     t = pars$t,
                                     eps.val = pars$eps,
                                     neigen = pars$ndim,
                                     maxdim = pars$ndim,
                                     delta = pars$delta
                                 )
        outdata <- diffres$X

        appl <- function(x) {
            appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
            proj <- if (inherits(x, "dimRedData")) x@data else x

            if (ncol(proj) != ncol(data@data))
                stop("x must have the same number of dimensions ",
                     "as the original data")

            dd <- sqrt(pdist2(proj, indata))

            appl.res <-
                diffusionMap::nystrom(diffres, dd, sigma = diffres$epsilon)
            dimnames(appl.res) <- list(
                rownames(x), paste0("diffMap", seq_len(ncol(outdata)))
            )

            new("dimRedData", data = appl.res, meta = appl.meta)
        }

        colnames(outdata) <- paste0("diffMap", seq_len(ncol(outdata)))

        return(new(
            "dimRedResult",
            data = new("dimRedData",
                       data = outdata,
                       meta = meta),
            org.data     = orgdata,
            apply        = appl,
            has.apply    = TRUE,
            has.org.data = keep.org.data,
            method       = "diffmap",
            pars         = pars
        ))
    })
)
