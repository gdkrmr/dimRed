#' Laplacian Eigenmaps
#'
#' An S4 Class implementing Laplacian Eigenmaps
#'
#' Laplacian Eigenmaps use a kernel and were originally developed to
#' separate non-convex clusters under the name spectral clustering.
#'
#' @template dimRedMethodSlots
#'
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#' \code{LaplacianEigenmaps} can take the following parameters:
#' \describe{
#'   \item{ndim}{the number of output dimensions.}
#'
#'   \item{sparse}{A character vector specifying hot to make the graph
#'    sparse, \code{"knn"} means that a K-nearest neighbor graph is
#'    constructed, \code{"eps"} an epsilon neighborhood graph is
#'    constructed, else a dense distance matrix is used.}
#'
#'   \item{knn}{The number of nearest neighbors to use for the knn graph.}
#'   \item{eps}{The distance for the epsilon neighborhood graph.}
#'
#'   \item{t}{Parameter for the transformation of the distance matrix
#'   by \eqn{w=exp(-d^2/t)}, larger values give less weight to
#'   differences in distance, \code{t == Inf} treats all distances != 0 equally.}
#'   \item{norm}{logical, should the normed laplacian be used?}
#' }
#'
#' @section Implementation:
#' Wraps around \code{\link[loe]{spec.emb}}.
#'
#' @references
#'
#' Belkin, M., Niyogi, P., 2003. Laplacian Eigenmaps for
#' Dimensionality Reduction and Data Representation. Neural
#' Computation 15, 1373.
#'
#' @examples
#' if(requireNamespace(c("loe", "RSpectra", "Matrix"), quietly = TRUE)) {
#'
#' dat <- loadDataSet("3D S Curve")
#' emb <- embed(dat, "LaplacianEigenmaps")
#' plot(emb@data@data)
#'
#' }
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @export LaplacianEigenmaps
#' @exportClass LaplacianEigenmaps
LaplacianEigenmaps <- setClass(
    "LaplacianEigenmaps",
    contains = "dimRedMethod",
    prototype = list(
        stdpars = list(ndim = 2, sparse = "knn", knn = 50, eps = 0.1,
                       t = Inf, norm = TRUE),
        fun = function (data, pars,
                        keep.org.data = TRUE) {
        chckpkg("loe")
        chckpkg("RSpectra")
        chckpkg("Matrix")

        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        indata <- data@data

        if (is.null(pars$d))     pars$d    <- dist
        if (is.null(pars$knn))   pars$knn  <- 50
        if (is.null(pars$ndim))  pars$ndim <- 2
        if (is.null(pars$t))     pars$t    <- Inf
        if (is.null(pars$norm))  pars$norm <- TRUE


        message(Sys.time(), ": Creating weight matrix")
        W <- if (pars$sparse == "knn") {
                 knng <- makeKNNgraph(indata, k = pars$knn, eps = 0,
                                      diag = TRUE)
                 if (is.infinite(pars$t)){
                     knng <- igraph::set_edge_attr(knng, name = "weight", value = 1)
                 } else {
                   ea <- igraph::edge_attr(knng, name = "weight")
                   knng <- igraph::set_edge_attr(
                     knng, name = "weight", value = exp( -(ea ^ 2) / pars$t ))
                 }
                 igraph::as_adj(knng, sparse = TRUE,
                                attr = "weight", type = "both")
             } else if (pars$sparse == "eps") {
                 tmp <- makeEpsSparseMatrix(indata, pars$eps)
                 tmp@x <- if (is.infinite(pars$t)) rep(1, length(tmp@i))
                          else exp(- (tmp@x ^ 2) / pars$t)
                 ## diag(tmp) <- 1
                 as(tmp, "dgCMatrix")
             } else {                        # dense case
                 tmp <- dist(indata)
                 tmp[] <- if (is.infinite(pars$t)) 1
                          else exp( -(tmp ^ 2) / pars$t)
                 tmp <- as.matrix(tmp)
                 diag(tmp) <- 1
                 tmp
             }

        ## we don't need to test for symmetry, because we know the
        ## matrix is symmetric
        D <- Matrix::Diagonal(x = Matrix::rowSums(W))
        L <- D - W
        ## for the generalized eigenvalue problem, we do not have a solver
        ## use A u = \lambda B u
        ## Lgen <- Matrix::Diagonal(x = 1 / Matrix::diag(D) ) %*% L
        ## but then we get negative eigenvalues and complex eigenvalues
        Lgen <- L
        message(Sys.time(), ": Eigenvalue decomposition")
        outdata <- if (pars$norm) {
                       DS <- Matrix::Diagonal(x = 1 / sqrt(Matrix::diag(D)))
                       RSpectra::eigs_sym(DS %*% Lgen %*% DS,
                                          k = pars$ndim + 1,
                                          sigma = -1e-5)
                   } else {
                       RSpectra::eigs_sym(Lgen,
                                          k = pars$ndim + 1,
                                          sigma = -1e-5)
                   }
        message("Eigenvalues: ", paste(format(outdata$values),
                                       collapse = " "))
        ## The eigenvalues are in decreasing order and we remove the
        ## smallest, which should be approx 0:
        outdata <- outdata$vectors[, order(outdata$values)[-1],
                                   drop = FALSE]

        if(ncol(outdata) > 0) {
          colnames(outdata) <- paste0("LEIM", seq_len(ncol(outdata)))
        } else {
          warning("no dimensions left, this is probably due to a badly conditioned eigenvalue decomposition.")
        }

        message(Sys.time(), ": DONE")
        return(new(
            "dimRedResult",
            data         = new("dimRedData",
                               data = outdata,
                               meta = meta),
            org.data     = orgdata,
            has.org.data = keep.org.data,
            method       = "leim",
            pars         = pars
        ))
        },
        requires = c("loe", "RSpectra", "Matrix"))
)
