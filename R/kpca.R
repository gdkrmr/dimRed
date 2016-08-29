

#' implements Kernel PCA for the use with dimRed
#'
#' uses \code{\link[kernlab]{kpca}} internally. Has methods for
#' forward and inverse calculations.
#'
#' @param data an object of class \code{\link{dimRedData-class}}.
#' @param pars an object of class \code{\link{dimRedMethodPars-class}}
#'
#' @return an object of class \code{\link{dimRedResult-class}}
#'
#' @examples
#'
#' dat <- loadDataSet("3D S Curve")
#' emb <- kpca@fun(dat, list(kernel = 'rbfdot',
#'                           kpar = list(sigma = 0.1),
#'                           ndim = 2))
#'
#' plot(emb@data@data)
#' 
#' @include dimRed-class.R
#' 
#' @export
kpca <- new('dimRedMethod',
            fun = function (data, pars = list(kernel = 'rbfdot',
                                              kpar = list(sigma = 0.1),
                                              ndim = 2),
                            keep.org.data = TRUE) {
    if(!requireNamespace('kernlab')) stop('require package "kernlab"')
    if(is.null(pars$ndim))
        pars$ndim <- 2
    
    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data

    res <- do.call(kernlab::kpca, c(list(x = indata), pars))

    kernel <- get_kernel_fun(pars$kernel, pars$kpar)
    
    # for the inverse:
    K_rev <- kernlab::kernelMatrix(kernel, res@rotated)
    diag(K_rev) <- 0.1 + diag(K_rev)
    dual_coef <- solve(K_rev, indata)        
    
    appl <- function (x) {
        appl.meta <- if(inherits(x, 'dimRedData')) x@meta else data.frame() 
        proj <- if(inherits(x, 'dimRedData')) x@data else x
         
        proj <- kernlab::predict(res, proj)
        colnames(proj) <- paste0("kPCA", 1:ncol(proj))

        new('dimRedData', data = proj, meta = appl.meta)
    }
    
    inv <- function (x) {
        appl.meta <- if(inherits(x, 'dimRedData')) x@meta else data.frame()
        proj <- if(inherits(x, 'dimRedData')) x@data else x
        
        resrot <- res@rotated[,1:ncol(proj)]
        rot <- kernlab::kernelMatrix(kernel, proj, resrot)
        proj <- rot %*% dual_coef

        new('dimRedData', data = proj, meta = appl.meta)
    }

    outdata <- res@rotated[,1:pars$ndim, drop = FALSE]
    colnames(outdata) <- paste0("kPCA", 1:ncol(outdata))
    
    return(
        new(
            'dimRedResult',
            data         = new('dimRedData',
                               data = outdata,
                               meta = meta),
            org.data     = orgdata,
            apply        = appl,
            inverse      = inv,
            has.org.data = keep.org.data,
            has.apply    = TRUE,
            has.inverse  = TRUE,
            method       = "kpca",
            pars         = pars
        )
    )
})



## get the kernel function out of the kernlab namespace:
get_kernel_fun <- function (kernel, pars) {
    if (!is(kernel,"kernel")) {
        if (is(kernel,"function")) {
            kernel <- deparse(substitute(kernel))
        } else {
            kernel <- get(kernel, asNamespace('kernlab'))
        }
        kernel <- do.call(kernel, pars)
    }
    return(kernel)
}
