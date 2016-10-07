#' Graph Embedding via the Kamada Kawai Algorithm
#'
#' Instance of \code{\link{dimRedMethod}} for the Kamada Kawai Algorithm.
#' 
#' For details see \code{\link[igraph]{layout_with_kk}}.
#' 
#' @examples
#' dat <- loadDataSet("Swiss Roll")
#' kk <- kamada_kawai@fun(dat, kamada_kawai@stdpars)
#' 
#' plot(kk@data@data)
#' 
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#'
#' @export
kamada_kawai <- new('dimRedMethod',
                    stdpars = list(ndim         = 2,
                                   knn          = 100,
                                   d            = dist,
                                   weight.trans = function (x) exp(-(x^2))),
                    fun = function (data, pars,
                                    keep.org.data = TRUE) {
    chckpkg('igraph')

    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data
    
    outdata <- em_graph_layout(
        indata,
        graph_em_method = igraph::layout_with_kk,
        knn = pars$knn,
        d = pars$d,
        ndim = pars$ndim,
        weight.trans = pars$weight.trans
    )

    colnames(outdata) <- paste0("KK", 1:ncol(outdata))

    return(new(
        'dimRedResult',
        data         = new('dimRedData',
                           data = outdata,
                           meta = meta),
        org.data     = orgdata,
        has.org.data = keep.org.data,
        method       = "graph_kk",
        pars         = pars        
    ))
})

#' Distributed Recursive Graph Layout
#'
#' Instance of \code{\link{dimRedMethod}} for the Distributed Recursive Graph Layout algorithm.
#' 
#' For details see \code{\link[igraph]{layout_with_drl}}
#' 
#' @examples
#'
#' dat <- loadDataSet("Swiss Roll")
#' drgl <- drl@fun(dat, drl@stdpars)
#' 
#' plot(drgl@data@data)
#' 
#' 
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @export
drl <- new('dimRedMethod',
           stdpars = list(ndim         = 2,
                          knn          = 100,
                          d            = dist,
                          weight.trans = function (x) exp(-(x^2))),
           fun = function (data, pars,
                           keep.org.data = TRUE) {
    chckpkg('igraph')
    
    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data
    
    outdata <- em_graph_layout(
        indata,
        graph_em_method = igraph::layout_with_drl,
        knn = pars$knn,
        d = pars$d,
        ndim = pars$ndim,
        weight.trans = pars$weight.trans
    )

    colnames(outdata) <- paste0("DrL", 1:ncol(outdata))

    return(new(
        'dimRedResult',
        data         = new('dimRedData',
                           data = outdata,
                           meta = meta),
        org.data     = orgdata,
        has.org.data = keep.org.data,
        method       = "graph_drl",
        pars         = pars
    ))
})

#' Fruchterman Reingold Graph Layout
#'
#' Instance of \code{\link{dimRedMethod}} for the Fruchterman Reingold Graph Layout algorithm.
#' 
#' For details see \code{\link[igraph]{layout_with_fr}}
#' 
#' @examples
#'
#' dat <- loadDataSet("Swiss Roll")
#' fr <- fruchterman_reingold@fun(dat, fruchterman_reingold@stdpars)
#' 
#' plot(fr@data@data)
#' 
#' 
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @export
fruchterman_reingold <- new('dimRedMethod',
                            stdpars = list(ndim         = 2,
                                           knn          = 100,
                                           d            = stats::dist,
                                           weight.trans = function (x) exp(-(x^2))),
                            fun = function (data, pars,
                                            keep.org.data = TRUE) {
    chckpkg('igraph')
                       
    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data
    
    outdata <- em_graph_layout(
        indata,
        graph_em_method = igraph::layout_with_fr,
        knn = pars$knn,
        d = pars$d,
        ndim = pars$ndim,
        weight.trans = pars$weight.trans
    )

    colnames(outdata) <- paste0("FR", 1:ncol(outdata))

    return(new(
        'dimRedResult',
        data         = new('dimRedData',
                           data = outdata,
                           meta = meta),
        org.data     = orgdata,
        has.org.data = keep.org.data,
        method       = "graph_fr",
        pars         = pars
    ))
})


em_graph_layout <- function(data, graph_em_method,
                            knn = 50, d = stats::dist,
                            ndim = 2, weight.trans = I){
  chckpkg('igraph')
  
  data.dist <- as.matrix(d(data))
  data.graph <- construct_knn_graph(data.dist, knn)

  embed_graph(data.graph, graph_em_method, ndim = ndim)
}

embed_graph <- function(graph, f, weight.trans = I, ndim = 2){
  f(graph, weights = weight.trans(igraph::E(graph)$weight), dim = ndim)
}


construct_knn_graph <- function (data.dist, knn) {
  chckpkg('igraph')
  chckpkg('coRanking')
  
  data.graph <- igraph::graph_from_adjacency_matrix(
    adjmatrix = data.dist,
    mode = 'undirected',
    weighted = T
  )

  if (is.infinite(knn) || is.na(knn))
    return(data.graph)
  ## else: remove all unnecessary edges
  data.rankm <- coRanking::rankmatrix(data.dist, input = 'dist')
  data.rankm.ind <- data.rankm <= knn + 1
  inds <- which(
    !(data.rankm.ind | t(data.rankm.ind)),
    arr.ind = TRUE
  )

  data.graph[ from = inds[,1], to = inds[,2] ] <- FALSE

  return(data.graph)
}

