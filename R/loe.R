
## this function produces segfaults and is super slow

## #' Local Ordinal Embedding
## #'
## #' Instance of \code{\link{dimRedMethod}} for Local Ordinal Embedding.
## #'
## #' For details see \code{\link[loe]{LOE}}
## #'
## #' @examples
## #' # for whatever reason the loe package has problems if I run this
## #' # with R CMD check, running it in the REPL works just fine
## #' dat <- loadDataSet("Iris")[sample(20)]
## #' loe <- LOE()
## #' emb <- loe@fun(dat, loe@stdpars)
## #'
## #' 
## #' plot(emb@data@data)
## #'
## #' @include dimRedResult-class.R
## #' @include dimRedMethod-class.R
## #' @export
## LOE <- setClass(
##     'LOE',
##     contains = 'dimRedMethod',
##     prototype = list(
##         stdpars = list(d = stats::dist, knn = 50, ndim = 2),
##         fun = function (data, pars,
##                         keep.org.data = TRUE) {
##         chckpkg('loe')

##         meta <- data@meta
##         orgdata <- if (keep.org.data) data@data else NULL
##         indata <- data@data

##         data.adj <- loe:::make.kNNG(as.matrix(pars$d(indata)), k = pars$knn)
##         outdata <- loe::LOE(data.adj, p = pars$ndim, method = "MM")$X

##         colnames(outdata) <- paste0("LOE", 1:ncol(outdata))

##         return(new(
##             'dimRedResult',
##             data         = new('dimRedData',
##                                data = outdata,
##                                meta = meta),
##             org.data     = orgdata,
##             has.org.data = keep.org.data,
##             method       = "loe",
##             pars         = pars
##         ))
##     })
## )
