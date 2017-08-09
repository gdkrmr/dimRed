#' @include dimRedResult-class.R
#' @include dimRedData-class.R

#' @export
setGeneric("quality",
           function (.data, ...) standardGeneric("quality"),
           valueClass = "numeric")

#' Quality Criteria for dimensionality reduction.
#'
#' A collection of functions to compute quality measures on
#' \code{\link{dimRedResult}} objects.
#'
#' @section Implemented methods:
#'
#' Method must be one of \code{"\link{Q_local}", "\link{Q_global}",
#' "\link{mean_R_NX}", "\link{total_correlation}",
#' "\link{cophenetic_correlation}", "\link{distance_correlation}",
#' "\link{reconstruction_rmse}"}
#'
#' @section Rank based criteria:
#'
#' \code{Q_local}, \code{Q_global}, and \code{mean_R_nx} are
#' quality criteria based on the Co-ranking matrix.  \code{Q_local}
#' and \code{Q_global} determine the local/global quality of the
#' embedding, while \code{mean_R_nx} determines the quality of the
#' overall embedding. They are parameter free and return a single
#' number. The object must include the original data.  The number
#' returns is in the range [0, 1], higher values mean a better
#' local/global embedding.
#'
#' @section Correlation based criteria:
#'
#' \code{total_correlation} calculates the sum of the mean squared
#' correlations of the original axes with the axes in reduced
#' dimensions, because some methods do not care about correlations
#' with axes, there is an option to rotate data in reduced space to
#' maximize this criterium. The number may be greater than one if more
#' dimensions are summed up.
#'
#' \code{cophenetic_correlation} calculate the correlation between the
#' lower triangles of distance matrices, the correlation and distance
#' methods may be specified. The result is in range [-1, 1].
#'
#' \code{distance_correlation} measures the independes of samples by
#' calculating the correlation of distances. For details see
#' \code{\link[energy]{dcor}}.
#'
#' @section Reconstruction error:
#'
#' \code{reconstruction_rmse} calculates the root mean squared error
#' of the reconstrucion. \code{object} requires an inverse function.
#'
#'
#' @references
#'
#' Lueks, W., Mokbel, B., Biehl, M., Hammer, B., 2011. How
#'     to Evaluate Dimensionality Reduction? - Improving the
#'     Co-ranking Matrix. arXiv:1110.3917 [cs].
#'
#' Szekely, G.J., Rizzo, M.L., Bakirov, N.K., 2007. Measuring and
#'     testing dependence by correlation of distances. Ann. Statist. 35,
#'     2769-2794. doi:10.1214/009053607000000505
#'
#' Lee, J.A., Peluffo-Ordonez, D.H., Verleysen, M., 2015. Multi-scale
#'     similarities in stochastic neighbour embedding: Reducing
#'     dimensionality while preserving both local and global
#'     structure. Neurocomputing, 169,
#'     246-261. doi:10.1016/j.neucom.2014.12.095
#'
#'
#'
#' @param .data object of class \code{dimRedResult}
#' @param .method character vector naming one of the methods
#' @param .mute what output from the embedding method should be muted.
#' @param ... the pameters, internally passed as a list to the
#'     quality method as \code{pars = list(...)}
#' @return a number
#'
#' @examples
#' \dontrun{
#' embed_methods <- dimRedMethodList()
#' quality_methods <- dimRedQualityList()
#' scurve <- loadDataSet("3D S Curve", n = 500)
#'
#' quality_results <- matrix(NA, length(embed_methods), length(quality_methods),
#'                               dimnames = list(embed_methods, quality_methods))
#' embedded_data <- list()
#'
#' for (e in embed_methods) {
#'   message("embedding: ", e)
#'   embedded_data[[e]] <- embed(scurve, e, .mute = c("message", "output"))
#'   for (q in quality_methods) {
#'     message("  quality: ", q)
#'     quality_results[e, q] <- tryCatch(
#'       quality(embedded_data[[e]], q),
#'       error = function (e) NA
#'     )
#'   }
#' }
#'
#' print(quality_results)
#' }
#' @author Guido Kraemer
#' @aliases quality quality.dimRedResult
#' @family Quality scores for dimensionality reduction
#' @describeIn quality Calculate a quality index from a dimRedResult object.
#' @export
setMethod(
    "quality",
    "dimRedResult",
    function (.data, .method = dimRedQualityList(),
              .mute = character(0), # c("output", "message"),
              ...) {
        method <-  match.arg(.method)

        methodFunction <- getQualityFunction(method)

        args <- c(list(object = .data), list(...))

        devnull <- if (Sys.info()["sysname"] != "Windows")
                       "/dev/null"
                   else
                       "NUL"
        if ("message" %in% .mute){
            devnull1 <- file(devnull,  "wt")
            sink(devnull1, type = "message")
            on.exit({
                sink(file = NULL, type = "message")
                close(devnull1)
            }, add = TRUE)
        }
        if ("output" %in% .mute) {
            devnull2 <- file(devnull,  "wt")
            sink(devnull2, type = "output")
            on.exit({
                sink()
                close(devnull2)
            }, add = TRUE)
        }

        do.call(methodFunction, args)
    }
)

getQualityFunction <- function (method) {
    switch(
        method,
        Q_local                = Q_local,
        Q_global               = Q_global,
        mean_R_NX              = mean_R_NX,
        AUC_lnK_R_NX           = AUC_lnK_R_NX,
        total_correlation      = total_correlation,
        cophenetic_correlation = cophenetic_correlation,
        distance_correlation   = distance_correlation,
        reconstruction_rmse    = reconstruction_rmse
    )
}


#' @export
setGeneric(
    "Q_local",
    function(object, ...) standardGeneric("Q_local"),
    valueClass = "numeric"
)


#' Method Q_local
#'
#' Calculate the Q_local score to assess the quality of a dimensionality reduction.
#'
#' @param object of class dimRedResult.
#' @param ndim use the first ndim columns of the embedded data for calculation.
#' @family Quality scores for dimensionality reduction
#' @aliases Q_local
#' @export
setMethod(
    "Q_local",
    "dimRedResult",
    function (object, ndim = getNDim(object)) {
        if (!object@has.org.data) stop("object requires original data")
        chckpkg("coRanking")

        Q <- coRanking::coranking(object@org.data,
                                  object@data@data[, seq_len(ndim), drop = FALSE])
        nQ <- nrow(Q)
        N <- nQ + 1

        Qnx <- diag(apply(apply(Q, 2, cumsum), 1, cumsum)) / seq_len(nQ) / N
        lcmc <- Qnx - seq_len(nQ) / nQ

        Kmax <- which.max(lcmc)

        Qlocal <- sum(lcmc[1:Kmax]) / Kmax
        return(as.vector(Qlocal))
    }
)

#' @export
setGeneric(
    "Q_global",
    function(object, ...) standardGeneric("Q_global"),
    valueClass = "numeric"
)

#' Method Q_global
#'
#' Calculate the Q_global score to assess the quality of a dimensionality reduction.
#'
#' @param object of class dimRedResult
#' @family Quality scores for dimensionality reduction
#' @aliases Q_global
#' @export
setMethod(
    "Q_global",
    "dimRedResult",
    function(object){
        if (!object@has.org.data) stop("object requires original data")
        chckpkg("coRanking")

        Q <- coRanking::coranking(object@org.data, object@data@data)
        nQ <- nrow(Q)
        N <- nQ + 1

        Qnx <- diag(apply(apply(Q, 2, cumsum), 1, cumsum)) / seq_len(nQ) / N
        lcmc <- Qnx - seq_len(nQ) / nQ

        Kmax <- which.max(lcmc)

        Qglobal <- sum(lcmc[(Kmax + 1):nQ]) / (N - Kmax)
        return(Qglobal)
    }
)

#' @export
setGeneric(
    "mean_R_NX",
    function(object, ...) standardGeneric("mean_R_NX"),
    valueClass = "numeric"
)

#' Method mean_R_NX
#'
#' Calculate the mean_R_NX score to assess the quality of a dimensionality reduction.
#'
#' @param object of class dimRedResult
#' @family Quality scores for dimensionality reduction
#' @aliases mean_R_NX
#' @export
setMethod(
    "mean_R_NX",
    "dimRedResult",
    function(object) mean(R_NX(object))
)

#' @export
setGeneric(
    "AUC_lnK_R_NX",
    function(object, ...) standardGeneric("AUC_lnK_R_NX"),
    valueClass = "numeric"
)

#' Method AUC_lnK_R_NX
#'
#' Calculate the Area under the R_NX(ln K), used in Lee et. al. (2013).
#'
#' @references
#'
#' Lee, J.A., Renard, E., Bernard, G., Dupont, P., Verleysen, M.,
#' 2013. Type 1 and 2 mixtures of Kullback-Leibler divergences as cost
#' functions in dimensionality reduction based on similarity
#' preservation. Neurocomputing. 112,
#' 92-107. doi:10.1016/j.neucom.2012.12.036
#'
#' @param object of class dimRedResult
#' @family Quality scores for dimensionality reduction
#' @aliases AUC_lnK_R_NX
#' @export
setMethod(
    "AUC_lnK_R_NX",
    "dimRedResult",
    function(object) {
        rnx <- R_NX(object)
        auc_lnK(rnx)
    }
)

auc_lnK <- function(rnx) {
    Ks <- seq_along(rnx)
    return (sum(rnx / Ks) / sum(1 / Ks))
    ## in my intuition this should be the following:
    ## N <- length(rnx)
    ## sum((rnx[-N] + rnx[-1]) / 2 * (log(2:N) - log(seq_len(N - 1))))
}


#' @export
setGeneric(
    "total_correlation",
    function(object, ...) standardGeneric("total_correlation"),
    valueClass = "numeric"
)

#' Method total_correlation
#'
#' Calculate the total correlation of the variables with the axes to
#' assess the quality of a dimensionality reduction.
#'
#' @param object of class dimRedResult
#' @param naxes the number of axes to use for optimization.
#' @param cor_method the correlation method to use.
#' @param is.rotated if FALSE the object is rotated.
#'
#' @family Quality scores for dimensionality reduction
#' @aliases total_correlation
#' @export
setMethod(
    "total_correlation",
    "dimRedResult",
    function(object,
             naxes = ndims(object),
             cor_method = "pearson",
             is.rotated = FALSE){

        if (!object@has.org.data) stop("object requires original data")
        if (length(naxes) != 1 || naxes < 1 || naxes > ncol(object@data@data))
            stop("naxes must specify the numbers of axes to optimize for, ",
                 "i.e. a single integer between 1 and ncol(object@data@data)")
        ## try to partially match cor_method:
        cor_methods <- c("pearson", "kendall", "spearman")
        cor_method <- cor_methods[pmatch(cor_method, cor_methods)]
        if (is.na(cor_method))
            stop("cor_method must match one of ",
                 "'pearson', 'kendall', or 'spearman', ",
                 "at least partially.")

        if (!is.rotated) {
            rotated_result <- maximize_correlation(
                object, naxes, cor_method
            )
        } else {
            rotated_result <- object
        }

        res <- 0
        for (i in 1:naxes)
            res <- res + mean(correlate(
                             rotated_result@data@data,
                             rotated_result@org.data,
                             cor_method
                         )[i, ] ^ 2)

        return(res)
    }
)

setGeneric("cophenetic_correlation",
           function(object, ...) standardGeneric("cophenetic_correlation"),
           valueClass = "numeric")

#' Method cophenetic_correlation
#'
#' Calculate the correlation between the distance matrices in high and
#' low dimensioal space.
#'
#' @param object of class dimRedResult
#' @param d the distance function to use.
#' @param cor_method The correlation method.
#' @aliases cophenetic_correlation
#' @family Quality scores for dimensionality reduction
#' @export
setMethod(
    "cophenetic_correlation",
    "dimRedResult",
    function(object, d = stats::dist, cor_method = "pearson"){
        ## if (missing(d)) d <- stats::dist
        ## if (missing(cor_method)) cor_method <- "pearson"
        if (!object@has.org.data) stop("object requires original data")
        cor_methods <- c("pearson", "kendall", "spearman")
        cor_method <- cor_methods[pmatch(cor_method, cor_methods)]
        if (is.na(cor_method))
            stop("cor_method must match one of ",
                 "'pearson', 'kendall', or 'spearman', ",
                 "at least partially.")

        d.org <- d(object@org.data)
        d.emb <- d(object@data@data)

        if (!inherits(d.org, "dist") || !inherits(d.emb, "dist"))
            stop("d must return a dist object")

        res <- correlate(
            d(object@org.data),
            d(object@data@data),
            cor_method
        )
        return(res)
    }
)

#' @export
setGeneric(
    "distance_correlation",
    function(object) standardGeneric("distance_correlation"),
    valueClass = "numeric"
)

#' Method distance_correlation
#'
#' Calculate the distance correlation between the distance matrices in
#' high and low dimensioal space.
#'
#' @param object of class dimRedResult
#' @aliases distance_correlation
#' @family Quality scores for dimensionality reduction
#' @export
setMethod(
    "distance_correlation",
    "dimRedResult",
    function(object){
        if (!object@has.org.data) stop("object requires original data")
        if (!requireNamespace("energy")) stop("package energy required.")

        energy::dcor(object@org.data, object@data@data)
    }
)



#' @export
setGeneric(
    "reconstruction_rmse",
    function(object) standardGeneric("reconstruction_rmse"),
    valueClass = "numeric"
)

#' Method reconstruction_rmse
#'
#' Calculate the reconstruction root mean squared error a dimensionality reduction, the method must have an inverse mapping.
#'
#' @param object of class dimRedResult
#' @aliases reconstruction_rmse
#' @family Quality scores for dimensionality reduction
#' @export
setMethod(
    "reconstruction_rmse",
    "dimRedResult",
    function(object){
        if (!object@has.org.data) stop("object requires original data")
        if (!object@has.inverse) stop("object requires an inverse function")

        recon <- object@inverse(object@data)

        sqrt(mean((recon@data - object@org.data) ^ 2))
    }
)

#' @rdname quality
#'
#' @export
dimRedQualityList <- function () {
    return(c("Q_local",
             "Q_global",
             "mean_R_NX",
             "AUC_lnK_R_NX",
             "total_correlation",
             "cophenetic_correlation",
             "distance_correlation",
             "reconstruction_rmse"))
}

#' @export
setGeneric(
    "R_NX",
    function(object) standardGeneric("R_NX"),
    valueClass = "numeric"
)

#' Method R_NX
#'
#' Calculate the R_NX score from Lee et. al. (2013) which shows the
#' neighborhood preservation for the Kth nearest neighbors,
#' corrected for random point distributions and scaled to range [0, 1].
#' @param object of class dimRedResult
#' @family Quality scores for dimensionality reduction
#' @aliases R_NX
#' @export
setMethod(
    "R_NX",
    "dimRedResult",
    function(object) {
        chckpkg("coRanking")
        if (!object@has.org.data) stop("object requires original data")

        Q <- coRanking::coranking(object@org.data, object@data@data)
        nQ <- nrow(Q)
        N <- nQ + 1

        Qnx <- diag(apply(apply(Q, 2, cumsum), 1, cumsum)) /
            seq_len(nQ) / N

        Rnx <- ((N - 1) * Qnx - seq_len(nQ)) /
            (N - 1 - seq_len(nQ))
        Rnx[-nQ]
    }
)

#' @export
setGeneric(
    "Q_NX",
    function(object, ...) standardGeneric("Q_NX"),
    valueClass = "numeric"
)

#' Method Q_NX
#'
#' Calculate the Q_NX score (Chen & Buja 2006, the notation in the
#' publication is M_k). Which is the fraction of points that remain inside
#' the same K-ary neighborhood in high and low dimensional space.
#'
#' @param object of class dimRedResult
#' @family Quality scores for dimensionality reduction
#' @aliases Q_NX
#' @export
setMethod(
    "Q_NX",
    "dimRedResult",
    function(object) {
        chckpkg("coRanking")

        Q <- coRanking::coranking(object@org.data, object@data@data)
        nQ <- nrow(Q)
        N <- nQ + 1

        Qnx <- diag(apply(apply(Q, 2, cumsum), 1, cumsum)) / seq_len(nQ) / N
        Qnx
    }
)

#'@export
setGeneric(
    "LCMC",
    function(object, ...) standardGeneric("LCMC"),
    valueClass = "numeric"
)

#' Method LCMC
#'
#' Calculates the Local Continuity Meta Criterion, which is
#' \code{\link{Q_NX}} adjusted for random overlap inside the K-ary
#' neighborhood.
#'
#' @param object of class dimRedResult
#' @family Quality scores for dimensionality reduction
#' @aliases LCMC
#' @export
setMethod(
    "LCMC",
    "dimRedResult",
        function(object) {
        chckpkg("coRanking")

        Q <- coRanking::coranking(object@org.data, object@data@data)
        nQ <- nrow(Q)
        N <- nQ + 1

        lcmc <- diag(apply(apply(Q, 2, cumsum), 1, cumsum)) /
            seq_len(nQ) / N -
            seq_len(nQ) / nQ
        lcmc
    }
)

rnx2qnx <- function(rnx, K = seq_along(rnx), N = length(rnx) + 1) {
    (rnx * (N - 1 - K) + K) / (N - 1)
}
qnx2rnx <- function(qnx, K = seq_along(qnx), N = length(qnx) + 1) {
    ((N - 1) * qnx - K) / (N - 1 - K)
}

#' @export
setGeneric(
  "reconstruction_error",
  function(object, ...) standardGeneric("reconstruction_error"),
  valueClass = "numeric"
)

#' Method reconstruction_error
#'
#' Calculate the error using only the first \code{n} dimensions of the embedded
#' data. \code{error_fun} can either be one of \code{c("rmse", "mae")} to
#' calculate the root mean square error or the mean absolute error respectively,
#' or a function that takes to equally sized vectors as input and returns a
#' single number as output.
#'
#' @param object of class dimRedResult
#' @param n a positive integer or vector of integers \code{<= ndims(object)}
#' @param error_fun a function or string indicating an error function, if
#'   indication a function it must take to matrices of the same size and return
#'   a scalar.
#' @return a vector of number with the same length as \code{n} with the
#'
#' @examples
#' \dontrun{
#' ir <- loadDataSet("Iris")
#' ir.drr <- embed(ir, "DRR", ndim = ndims(ir))
#' ir.pca <- embed(ir, "PCA", ndim = ndims(ir))
#'
#' rmse <- data.frame(
#'   rmse_drr = reconstruction_error(ir.drr),
#'   rmse_pca = reconstruction_error(ir.pca)
#' )
#'
#' matplot(rmse, type = "l")
#' plot(ir)
#' plot(ir.drr)
#' plot(ir.pca)
#' }
#' @author Guido Kraemer
#' @family Quality scores for dimensionality reduction
#' @aliases reconstruction_error
#' @export
setMethod(
  "reconstruction_error",
  c("dimRedResult"),
  function (object, n = seq_len(ndims(object)), error_fun = "rmse") {
    if (any(n > ndims(object))) stop("n > ndims(object)")
    if (any(n < 1))             stop("n < 1")

    ef <- if (inherits(error_fun, "character")) {
      switch(
        error_fun,
        rmse = rmse,
        mae  = mae
      )
    } else if (inherits(error_fun, "function")) {
      error_fun
    } else {
      stop("error_fun must be a string or function, see documentation for details")
    }

    res <- numeric(length(n))
    org <- getData(getOrgData(object))
    for (i in seq_along(n)) {
      rec <- getData(inverse(
        object, getData(getDimRedData(object))[, seq_len(n[i]), drop = FALSE]
      ))
      res[i] <- ef(org, rec)
    }
    res
  }
)

rmse <- function (x1, x2) sqrt(mean((x1 - x2) ^ 2))
mae  <- function (x1, x2) mean(abs(x1 - x2))
