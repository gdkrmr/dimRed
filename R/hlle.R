#' Hessian Locally Linear Embedding
#'
#' An S4 Class implementing Hessian Locally Linear Embedding (HLLE)
#'
#' HLLE uses local hessians to approximate the curvines and is an
#' extension to non-convex subsets in lowdimensional space.
#'
#' @template dimRedMethodSlots
#'
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#' HLLE can take the following parameters:
#' \describe{
#'   \item{knn}{neighborhood size}
#'   \item{ndim}{number of output dimensions}
#' }
#'
#' @section Implementation:
#' Own implementation, sticks to the algorithm in Donoho and Grimes
#' (2003). Makes use of sparsity to speed up final embedding.
#'
#' @references
#' Donoho, D.L., Grimes, C., 2003. Hessian eigenmaps: Locally linear
#' embedding techniques for high-dimensional data. PNAS 100,
#' 5591-5596. doi:10.1073/pnas.1031596100
#'
#' @examples
#' dat <- loadDataSet("3D S Curve", n = 300)
#' emb <- embed(dat, "HLLE", knn = 15)
#' plot(emb, type = "2vars")
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @export HLLE
#' @exportClass HLLE
HLLE <- setClass(
    "HLLE",
    contains = "dimRedMethod",
    prototype = list(
        stdpars = list(knn = 50, ndim = 2),
        fun = function(data, pars,
                       keep.org.data = TRUE) {
        chckpkg("RSpectra")
        chckpkg("Matrix")
        chckpkg("RANN")

        if (pars$ndim < 2) stop("ndim must be 2 or larger.")

        if (is.null(pars$knn))   pars$knn  <- 50
        if (is.null(pars$ndim))  pars$ndim <- 2

        indata <- data@data
        n <- nrow(indata)
        hs <- pars$ndim * (pars$ndim + 1) / 2
        W <- Matrix::sparseMatrix(i = numeric(0),
                                  j = numeric(0),
                                  x = numeric(0),
                                  dims = c(n, hs * n))
        ii <- jj <- ww <- list()
        ## Identify neighbors:
        message(Sys.time(), ": Finding nearest neighbors", sep = "")
        nnidx <- RANN::nn2(data = indata, query = indata, k = pars$knn + 1,
                           treetype = "kd", "standard", eps = 0)$nn.idx#[, -1]
        message(Sys.time(), ": Calculating Hessian", sep = "")
        for (i in seq_len(n)) {
            cat(i, "/", n, "\r", sep = "")
            ## get neighborhood
            Nui <- indata[nnidx[i, ], , drop = FALSE]

            ## Form tangent coordinates:
            Nui <- sweep(Nui, 2, colMeans(Nui), "-")
            tc <- svd(Nui, nu = pars$ndim, nv = 0)$u

            ## Develop Hessian Estimator
            Xi <- cbind(
                1, tc, tc ^ 2,
                apply(combn(seq_len(pars$ndim), 2), 2,
                      function(x) tc[, x[1]] * tc[, x[2]])
            )
            tHi <- qr.Q(qr(Xi))[, -(1:(pars$ndim + 1)),
                                drop = FALSE]

            ## Add quadratic form to hessian
            ii[[i]] <- rep(nnidx[i, ], hs)
            jj[[i]] <- rep((i - 1) * hs + (1:hs), each = ncol(nnidx))
            ww[[i]] <- as.vector(tHi)
        }
        H <- as(Matrix::tcrossprod(Matrix::spMatrix(
            i = unlist(ii, FALSE, FALSE),
            j = unlist(jj, FALSE, FALSE),
            x = unlist(ww, FALSE, FALSE),
            nrow = n, ncol = n * hs)
        ), "dgCMatrix")

        ## Find null space:
        message(Sys.time(), ": Embedding", sep = "")
        ## eigs and eigs_sym converges much more reliably and faster
        ## with sigma = -eps than with which = "L*"
        outdata <- RSpectra::eigs_sym(H, k = pars$ndim + 1, sigma = -1e-5)

        message(paste(c("Eigenvalues:", format(outdata$values)),
                      collapse = " "))
        outdata <- outdata$vectors[, order(outdata$values)[-1], drop = FALSE]

        colnames(outdata) <- paste0("HLLE", seq_len(ncol(outdata)))

        message(Sys.time(), ": DONE", sep = "")
        return(new(
            "dimRedResult",
            data         = new("dimRedData",
                               data = outdata,
                               meta = data@meta),
            org.data     = if (keep.org.data) data@data else NULL,
            has.org.data = keep.org.data,
            method       = "HLLE",
            pars         = pars
        ))
        },
        requires = c("RSpectra", "Matrix", "RANN"))
)
