#' Local ordinal embedding
#'
#' fit a local ordinal embedding
#'
#' for details see \code{\link[loe]{LOE}}
#'
#' @examples
#' \dontrun{
#' # for whatever reason the loe package has problems if I run this
#' # with R CMD check, running it in the REPL works just fine
#' dat <- loadDataSet("3D S Curve", n = 200)
#' emb <- loe@fun(dat)
#'
#' 
#' plot(emb@data@data)
#' }
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' 
#' @export
loe <- new('dimRedMethod',
           stdpars = list(d = dist, knn = 50, ndim = 2),
           fun = function (data, pars,
                           keep.org.data = TRUE) {
    if(!requireNamespace('loe')) stop('require the loe package')

    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data

    data.adj <- loe:::make.kNNG(as.matrix(pars$d(indata)), k = pars$knn)
    outdata <- loe::LOE(data.adj, p = pars$ndim, method = "MM")$X

    colnames(outdata) <- paste0("LOE", 1:ncol(outdata))

    return(new(
        'dimRedResult',
        data         = new('dimRedData',
                           data = outdata,
                           meta = meta),
        org.data     = orgdata,
        has.org.data = keep.org.data,
        method       = "loe",
        pars         = pars
    ))
})
