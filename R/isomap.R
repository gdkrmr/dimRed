#' Isomap embedding
#'
#' An S4 Class implementing the Isomap Algorithm
#'
#' The Isomap algorithm approximates a manifold using geodesic
#' distances on a k nearest neighbor graph. Then classical scaling is
#' performed on the resulting distance matrix.
#'
#' @template dimRedMethodSlots
#'
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#' Isomap can take the following parameters:
#' \describe{
#'   \item{knn}{The number of nearest neighbors in the graph. Defaults to 50.}
#'   \item{ndim}{The number of embedding dimensions, defaults to 2.}
#' }
#'
#' @section Implementation:
#'
#' The dimRed package uses its own implementation of Isomap which also
#' comes with an out of sample extension (known as landmark
#' Isomap). The default Isomap algorithm scales computationally not
#' very well, the implementation here uses \code{\link[RANN]{nn2}} for
#' a faster search of the neares neighbors.  If data are too large it
#' may be useful to fit a subsample of the data and use the
#' out-of-sample extension for the other points.
#'
#' @examples
#' dat <- loadDataSet("3D S Curve", n = 500)
#'
#' ## use the S4 Class directly:
#' isomap <- Isomap()
#' emb <- isomap@fun(dat, isomap@stdpars)
#'
#' ## or simpler, use embed():
#' samp <- sample(nrow(dat), size = 200)
#' emb2 <- embed(dat[samp], "Isomap", mute = NULL, knn = 10)
#' emb3 <- emb2@apply(dat[-samp])
#'
#' plot(emb2, type = "2vars")
#' plot(emb3, type = "2vars")
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @export Isomap
#' @exportClass Isomap
Isomap <- setClass(
    "Isomap",
    contains = "dimRedMethod",
    prototype = list(
        stdpars = list(knn = 50,
                       ndim = 2,
                       get_geod = FALSE),
        fun = function (data, pars,
                        keep.org.data = TRUE) {
        message(Sys.time(), ": Isomap START")
        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        indata <- data@data

        if (is.null(pars$eps))      pars$eps <- 0
        if (is.null(pars$get_geod)) pars$get_geod <- FALSE

        ## geodesic distances
        message(Sys.time(), ": constructing knn graph")
        knng <- makeKNNgraph(x = indata, k = pars$knn, eps = pars$eps)
        message(Sys.time(), ": calculating geodesic distances")
        geodist <- igraph::distances(knng, algorithm = "dijkstra")
        message(Sys.time(), ": cmdscale")
        ## TODO: add regularization
        k <- geodist ^ 2
        k <- .Call(stats:::C_DoubleCentre, k)
        k <- - k / 2
        ## TODO: explicit symmetrizing
        ## TODO: return eigenvectors?
        e <- RSpectra::eigs_sym(k, pars$ndim, which = "LA",
                                opts = list(retvec = TRUE))
        e_values <- e$values
        e_vectors <- e$vectors
        neig <- sum(e_values > 0)
        if (neig < pars$ndim) {
          warning("Isomap: eigenvalues < 0, returning less dimensions!")
          e_values <- e_values[seq_len(neig)]
          e_vectors <- e_vectors[, seq_len(neig), drop = FALSE]
        }

        e_vectors <- e_vectors * rep(sqrt(e_values), each = nrow(e_vectors))

        colnames(e_vectors) <- paste("iso", seq_len(neig))

        appl <- function (x) {
            message(Sys.time(), ": L-Isomap embed START")
            appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
            indata    <- if (inherits(x, "dimRedData")) x@data else x

            if (ncol(indata) != ncol(data@data))
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
            dammu <- sweep(lgeodist ^ 2, 2, colMeans(geodist ^ 2), "-")
            Lsharp <- sweep(e_vectors, 2, e_values, "/")
            out <- -0.5 * (dammu %*% Lsharp)

            message(Sys.time(), ": DONE")
            return(new("dimRedData", data = out, meta = appl.meta))
        }

        return(new(
            "dimRedResult",
            data         = new("dimRedData",
                               data = e_vectors,
                               meta = meta),
            org.data     = orgdata,
            has.org.data = keep.org.data,
            apply        = appl,
            has.apply    = TRUE,
            method       = "Isomap",
            pars         = pars,
            other.data   = if (pars$get_geod) list(geod = as.dist(geodist))
                           else               list()
        ))


    })
)


## input data(matrix or data frame) return knn graph implements
## "smart" choices on RANN::nn2 parameters we ignore radius search
## TODO: find out a good limit to switch from kd to bd trees COMMENT:
## bd trees are buggy, they dont work if there are duplicated data
## points and checking would neutralize the performance gain, so bd
## trees are not really usable.

makeKNNgraph <- function (x, k, eps = 0, diag = FALSE){
    ## requireNamespace("RANN")
    ## requireNamespace("igraph")

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
    ## it is not really pretty to create a
    ## directed graph first and then make it undirected.
    g <- igraph::make_empty_graph(M, directed = TRUE)
    g[from = if (diag) rep(seq_len(M), times = k + 1)
             else      rep(seq_len(M), times = k),
      to   = if (diag) as.vector(nn2res$nn.idx)
             else      as.vector(nn2res$nn.idx[, -1]),
      attr = "weight"] <-
        if (diag)  as.vector(nn2res$nn.dists)
        else as.vector(nn2res$nn.dists[, -1])

    return(igraph::as.undirected(g, mode = "collapse", edge.attr.comb = "first"))
}

## the original isomap method I'll keep it here for completeness:
## isomap <- new("dimRedMethod",
##               stdpars = list(knn  = 50,
##                              d    = dist,
##                              ndim = 2)
##               fun = function (data, pars,
##                               keep.org.data = TRUE) {
##     chckpkg("vegan")

##     meta <- data@meta
##     orgdata <- if (keep.org.data) data@data else NULL
##     indata <- data@data

##     outdata <- vegan::isomap(pars$d(indata),
##                              ndim = pars$ndim,
##                              k = pars$knn)$points

##     colnames(outdata) <- paste0("Iso", 1:ncol(outdata))

##     return(new(
##         "dimRedResult",
##         data         = new("dimRedData",
##                            data = outdata,
##                            meta = meta),
##         org.data     = orgdata,
##         has.org.data = keep.org.data,
##         method       = "isomap",
##         pars         = pars
##     ))
## })
