#' Graph Embedding via the Kamada Kawai Algorithm
#'
#' An S4 Class implementing the Kamada Kawai Algorithm for graph embedding.
#'
#' Graph embedding algorithms se the data as a graph. Between the
#' nodes of the graph exist attracting and repelling forces which can
#' be modeled as electrical fields or springs connecting the
#' nodes. The graph is then forced into a lower dimensional
#' representation that tries to represent the forces betweent he nodes
#' accurately by minimizing the total energy of the attracting and
#' repelling forces.
#'
#' @template dimRedMethodSlots
#'
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#' KamadaKawai can take the following parameters:
#' \describe{
#'   \item{ndim}{The number of dimensions, defaults to 2. Can only be 2 or 3}
#'   \item{knn}{Reduce the graph to keep only the neares neighbors. Defaults to 100.}
#'   \item{d}{The distance function to determine the weights of the graph edges. Defaults to euclidean distances.}
#' }
#'
#' @section Implementation:
#' Wraps around \code{\link[igraph]{layout_with_kk}}. The parameters
#' maxiter, epsilon and kkconst are set to the default values and
#' cannot be set, this may change in a future release. The DimRed
#' Package adds an extra sparsity parameter by constructing a knn
#' graph which also may improve visualization quality.
#'
#' @examples
#' dat <- loadDataSet("Swiss Roll", n = 500)
#' kamada_kawai <- KamadaKawai()
#' kk <- kamada_kawai@fun(dat, kamada_kawai@stdpars)
#'
#' plot(kk@data@data)
#'
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @export KamadaKawai
#' @exportClass KamadaKawai
KamadaKawai <- setClass(
    "KamadaKawai",
    contains = "dimRedMethod",
    prototype = list(
        stdpars = list(ndim         = 2,
                       knn          = 100,
                       d            = stats::dist),
        fun = function (data, pars,
                        keep.org.data = TRUE) {
        chckpkg("igraph")

        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        indata <- data@data

        outdata <- em_graph_layout(
            indata,
            graph_em_method = igraph::layout_with_kk,
            knn             = pars$knn,
            d               = pars$d,
            ndim            = pars$ndim,
            weight.trans    = I #pars$weight.trans
        )

        colnames(outdata) <- paste0("KK", 1:ncol(outdata))

        return(new(
            "dimRedResult",
            data         = new("dimRedData",
                               data = outdata,
                               meta = meta),
            org.data     = orgdata,
            has.org.data = keep.org.data,
            method       = "graph_kk",
            pars         = pars
        ))
    })
)


#' Distributed Recursive Graph Layout
#'
#' An S4 Class implementing Distributed recursive Graph Layout.
#'
#' DrL uses a complex algorithm to avoid local minima in the graph
#' embedding which uses several steps.
#'
#' @template dimRedMethodSlots
#'
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#' DrL can take the following parameters:
#' \describe{
#'   \item{ndim}{The number of dimensions, defaults to 2. Can only be 2 or 3}
#'   \item{knn}{Reduce the graph to keep only the neares neighbors. Defaults to 100.}
#'   \item{d}{The distance function to determine the weights of the graph edges. Defaults to euclidean distances.}
#' }
#'
#' @section Implementation:
#' Wraps around \code{\link[igraph]{layout_with_drl}}. The parameters
#' maxiter, epsilon and kkconst are set to the default values and
#' cannot be set, this may change in a future release. The DimRed
#' Package adds an extra sparsity parameter by constructing a knn
#' graph which also may improve visualization quality.
#'
#' @examples
#' dat <- loadDataSet("Swiss Roll", n = 500)
#'
#' ## use the S4 Class directly:
#' drl <- DrL()
#' emb <- drl@fun(dat, drl@stdpars)
#'
#' ## simpler, use embed():
#' emb2 <- embed(dat, "DrL")
#'
#'
#' plot(emb)
#'
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @export DrL
#' @exportClass DrL
DrL <- setClass(
    "DrL",
    contains = "dimRedMethod",
    prototype = list(
        stdpars = list(ndim         = 2,
                       knn          = 100,
                       d            = stats::dist),
        fun = function (data, pars,
                        keep.org.data = TRUE) {
        chckpkg("igraph")

        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        indata <- data@data

        outdata <- em_graph_layout(
            indata,
            graph_em_method = igraph::layout_with_drl,
            knn = pars$knn,
            d = pars$d,
            ndim = pars$ndim,
            weight.trans = I #pars$weight.trans
        )

        colnames(outdata) <- paste0("DrL", 1:ncol(outdata))

        return(new(
            "dimRedResult",
            data         = new("dimRedData",
                               data = outdata,
                               meta = meta),
            org.data     = orgdata,
            has.org.data = keep.org.data,
            method       = "graph_drl",
            pars         = pars
        ))
    })
)

#' Fruchterman Reingold Graph Layout
#'
#' An S4 Class implementing the Fruchterman Reingold Graph Layout
#' algorithm.
#'
#' @template dimRedMethodSlots
#'
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#' \describe{
#'   \item{ndim}{The number of dimensions, defaults to 2. Can only be 2 or 3}
#'   \item{knn}{Reduce the graph to keep only the neares neighbors. Defaults to 100.}
#'   \item{d}{The distance function to determine the weights of the graph edges. Defaults to euclidean distances.}
#' }
#'
#' @section Implementation:
#' Wraps around \code{\link[igraph]{layout_with_fr}}, see there for
#' details. The Fruchterman Reingold algorithm puts the data into
#' a circle and puts connected points close to each other.
#'
#' @examples
#' dat <- loadDataSet("Swiss Roll", n = 100)
#'
#' ## use the S4 Class directly:
#' fruchterman_reingold <- FruchtermanReingold()
#' pars <- fruchterman_reingold@stdpars
#' pars$knn <- 5
#' emb <- fruchterman_reingold@fun(dat, pars)
#'
#' ## simpler, use embed():
#' emb2 <- embed(dat, "FruchtermanReingold", knn = 5)
#'
#' plot(emb, type = "2vars")
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @export FruchtermanReingold
#' @exportClass FruchtermanReingold
FruchtermanReingold <- setClass(
    "FruchtermanReingold",
    contains = "dimRedMethod",
    prototype = list(
        stdpars = list(ndim         = 2,
                       knn          = 100,
                       d            = stats::dist),
        fun = function (data, pars,
                        keep.org.data = TRUE) {
        chckpkg("igraph")

        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        indata <- data@data

        outdata <- em_graph_layout(
            indata,
            graph_em_method = igraph::layout_with_fr,
            knn = pars$knn,
            d = pars$d,
            ndim = pars$ndim,
            weight.trans = I #pars$weight.trans
        )

        colnames(outdata) <- paste0("FR", 1:ncol(outdata))

        return(new(
            "dimRedResult",
            data         = new("dimRedData",
                               data = outdata,
                               meta = meta),
            org.data     = orgdata,
            has.org.data = keep.org.data,
            method       = "graph_fr",
            pars         = pars
        ))
    })
)

em_graph_layout <- function(data, graph_em_method,
                            knn = 50, d = stats::dist,
                            ndim = 2, weight.trans = I){
  chckpkg("igraph")

  data.dist <- as.matrix(d(data))
  data.graph <- construct_knn_graph(data.dist, knn)

  embed_graph(data.graph, graph_em_method, ndim = ndim)
}

embed_graph <- function(graph, f, weight.trans = I, ndim = 2){
  f(graph, weights = weight.trans(igraph::E(graph)$weight), dim = ndim)
}


construct_knn_graph <- function (data.dist, knn) {
  chckpkg("igraph")
  chckpkg("coRanking")

  data.graph <- igraph::graph_from_adjacency_matrix(
    adjmatrix = data.dist,
    mode = "undirected",
    weighted = T
  )

  if (is.infinite(knn) || is.na(knn))
    return(data.graph)
  ## else: remove all unnecessary edges
  data.rankm <- coRanking::rankmatrix(data.dist, input = "dist")
  data.rankm.ind <- data.rankm <= knn + 1
  inds <- which(
    !(data.rankm.ind | t(data.rankm.ind)),
    arr.ind = TRUE
  )

  data.graph[ from = inds[, 1], to = inds[, 2] ] <- FALSE

  return(data.graph)
}

