#' Isomap embedding
#'
#' Instance of \code{\link{dimRedMethod}} for Isomap.
#'
#' Create a k nearest neighbor graph, compute geodesic distance on the
#' graph, use classical scaling for the embedding.
#' The landmark version is implemented via \code{isomap@apply()}.
#' 
#' 
#' Parameters to set are
#' \itemize{
#' \item k. The number of neighbors
#' \item ndim. The number of dimensions
#' \item eps. if larger than 0, approximate the nearest neighbors.
#' }
#'
#'
#' 
#' @examples
#' dat <- loadDataSet("3D S Curve")
#' emb <- isomap@fun(dat, isomap@stdpars)
#'
#' plot(emb@data@data)
#' 
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @import RANN
#' @import igraph
#' 
#' @export
isomap <- new("dimRedMethod",
              stdpars = list(knn = 50,
                             ndim = 2),
              fun = function (data, pars,
                              keep.org.data = TRUE) {
    message(Sys.time(), ": Isomap START")
    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data

    if (is.null(pars$eps)) pars$eps <- 0
    
    ## geodesic distances
    message(Sys.time(), ": constructing knn graph")
    knng <- makeKNNgraph(x = indata, k = pars$knn, eps = pars$eps)
    message(Sys.time(), ": calculating geodesic distances")
    geodist <- igraph::distances(knng, algorithm = "dijkstra")
    message(Sys.time(), ": cmdscale")
    cmdout <- stats::cmdscale(geodist, k = pars$ndim, eig = TRUE)

    message(Sys.time(), ": post processing")
    neig <- sum(cmdout$eig > 0)
    if(neig < pars$ndim) {
        warning("Isomap: eigenvalues < 0, returning less dimensions!")
        cmdout$points <- cmdout$points[, seq_len(neig)]
        cmdout$eig <- cmdout$eig[seq_len(neig)]
    } else {
        cmdout$eig <- cmdout$eig[seq_len(pars$ndim)]
    }

    appl <- function (x) {
        message(Sys.time(), ": L-Isomap embed START")
        appl.meta <- if(inherits(x, 'dimRedData')) x@meta else data.frame()
        indata      <- if(inherits(x, 'dimRedData')) x@data else x

        if(ncol(indata) != ncol(data@data))
            stop("x must have the same number of dimensions as the original data")

        nindata <- nrow(indata)
        norg <- nrow(orgdata)

        message(Sys.time(), ": constructing knn graph")
        lknng <- makeKNNgraph(rbind(indata, orgdata),
                              k = pars$knn, eps = pars$eps)
        message(Sys.time(), ": calculating geodesic distances")
        lgeodist <- igraph::distances(lknng,
                                      seq_len(nindata),
                                      nindata + seq_len(norg))

        message(Sys.time(), ": embedding")
        dammu <- sweep(lgeodist^2, 2, colMeans(geodist^2), "-")
        Lsharp <- sweep(cmdout$points, 2, cmdout$eig, "/")
        out <- -0.5 * (dammu %*% Lsharp)
                                                                                    
        message(Sys.time(), ": DONE")
        return(new('dimRedData', data = out, meta = appl.meta))    
    }
    
    return(new(
        'dimRedResult',
        data         = new('dimRedData',
                           data = cmdout$points,
                           meta = meta),
        org.data     = orgdata,
        has.org.data = keep.org.data,
        apply        = appl,
        has.apply    = TRUE,
        method       = "isomap",
        pars         = pars
    ))
    
    
})

## input data(matrix or data frame) return knn graph implements
## "smart" choices on RANN::nn2 parameters we ignore radius search
## TODO: find out a good limit to switch from kd to bd trees COMMENT:
## bd trees are buggy, they dont work if there are duplicated data
## points and checking would neutralize the performance gain, so bd
## trees are not really usable.

#' @import igraph
#' @import RANN
makeKNNgraph <- function (x, k, eps = 0){
    requireNamespace("RANN")
    requireNamespace("igraph")

    ## consts
    INF_VAL <- 1.340781e+15
    NA_IDX  <- 0
    BDKD_LIM <- 1000000                 #todo: figure out a good value here

    ## select parameters
    M <- nrow(x)
    treetype <- "kd"                # if (M < BDKD_LIM) "kd" else "bd"
                                    # see:
                                    # https://github.com/jefferis/RANN/issues/19
    searchtype <- if (eps == 0) "standard" else "priority"

    ## RANN::nn2 returns the points in data with respect to query
    ## e.g. the rows in the output are the points in query and the
    ## columns the points in data.
    nn2res <- RANN::nn2(data = x, query = x, k = k + 1, treetype = treetype,
                        searchtype = searchtype, eps = eps)

    ## create graph: the first ny nodes will be y, the last nx nodes
    ## will be x, if x != y
    g <- igraph::make_empty_graph(M, directed = FALSE)
    g[from = rep(seq_len(M), times = k),
      to   = as.vector(nn2res$nn.idx[, -1]),
      attr = "weight"] <- as.vector(nn2res$nn.dists[,-1])

    return(g) 
}

## the original isomap method I'll keep it here for completeness: 
## isomap <- new('dimRedMethod',
##               stdpars = list(knn  = 50,
##                              d    = dist,
##                              ndim = 2)
##               fun = function (data, pars,
##                               keep.org.data = TRUE) {
##     if(!requireNamespace('vegan')) stop('require the vegan package')

##     meta <- data@meta
##     orgdata <- if (keep.org.data) data@data else NULL
##     indata <- data@data

##     outdata <- vegan::isomap(pars$d(indata),
##                              ndim = pars$ndim,
##                              k = pars$knn)$points

##     colnames(outdata) <- paste0("Iso", 1:ncol(outdata))

##     return(new(
##         'dimRedResult',
##         data         = new('dimRedData',
##                            data = outdata,
##                            meta = meta),
##         org.data     = orgdata,
##         has.org.data = keep.org.data,
##         method       = "isomap",
##         pars         = pars
##     ))
## })
