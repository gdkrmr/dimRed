#' Independent Component Analysis
#'
#' An S4 Class implementing the FastICA algorithm for Indepentend
#' Component Analysis.
#'
#' ICA is used for blind signal separation of different sources. It is
#' a linear Projection.
#'
#' @template dimRedMethodSlots
#' 
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#' FastICA can take the following parameters:
#' \describe{
#'   \item{ndim}{The number of output dimensions. Defaults to \code{2}}
#' }
#'
#' @section Implementation:
#' Wraps around \code{\link[fastICA]{fastICA}}. FastICA uses a very
#' fast approximation for negentropy to estimate statistical
#' independences between signals. Because it is a simple
#' rotation/projection, forward and backward functions can be given.
#' 
#'
#' @examples
#' dat <- loadDataSet("3D S Curve")
#'
#' ## use the S4 Class directly:
#' fastica <- FastICA()
#' emb <- fastica@fun(dat, pars = list(ndim = 2))
#'
#' ## simpler, use embed():
#' emb2 <- embed(dat, "FastICA", ndim = 2)
#'
#' 
#' plot(emb@data@data)
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @export FastICA
#' @exportClass FastICA
FastICA <- setClass(
    "FastICA",
    contains = "dimRedMethod",
    prototype = list(
        stdpars = list(ndim = 2),
        fun = function (data,
                        pars,
                        keep.org.data = TRUE) {
        chckpkg("fastICA")
        
        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        orgdata.colmeans <- colMeans(orgdata)
        indata <- data@data

        res <- fastICA::fastICA(indata, n.comp = pars$ndim, method = "C")

        outdata <- res$S
        colnames(outdata) <- paste0("ICA", 1:ncol(outdata))

        appl <- function(x){
            appl.meta <- if (inherits(x, "dimRedData"))
                             x@meta
                         else   
                             matrix(numeric(0), 0,0)
            
            proj <- if (inherits(x, "dimRedData"))
                        x@data
                    else 
                        x
            
            out <- scale(proj, center = orgdata.colmeans, scale = FALSE)%*%res$K%*%res$W
            return(new("dimRedData", data = out, meta = appl.meta))
        }

        inv <- function(x){
            appl.meta <- if (inherits(x, "dimRedData"))
                             x@meta
                         else  
                             matrix(numeric(0), 0,0)
            
            proj <- if (inherits(x, "dimRedData"))
                        x@data
                    else
                        x
            
            out <- scale(proj%*%res$A[1:ncol(proj),], center = -orgdata.colmeans, scale = FALSE)
            reproj <- new("dimRedData", data = out, meta = appl.meta)
            return(reproj)
        }
        
        
        return(new(
            "dimRedResult",
            data         = new("dimRedData",
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
)

