#' Rank based quality criteria.
#'
#' A collection of functions to compute quality measures for
#' \code{\link{dimRedResult}} objects.
#'
#' @section Rank based criteria:
#'
#' The \code{Q_local}, \code{Q_global}, and \code{mean_R_nx} are
#' quality criteria based on the Co-ranking matrix.  \code{Q_local}
#' and \code{Q_global} determine the local/global quality of the
#' embedding, while \code{mean_R_nx} determines the quality of the
#' overall embedding. They are parameter free and return a single
#' number. The object must include the original data.  The number
#' returns is in the range [0,1], higher values mean a better
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
#' @param object a dimRedResult object that includes the original data.
#' @return a single number.
#'
#' @references
#' Lueks, W., Mokbel, B., Biehl, M., Hammer, B., 2011. How
#'     to Evaluate Dimensionality Reduction? - Improving the
#'     Co-ranking Matrix. arXiv:1110.3917 [cs].
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
#' @author Guido Kraemer
#' @aliases Q_local Q_global total_correlation cophenetic_correlation
#'     distance_correlation reconstruction_rmse
#' @include dimRedResult-class.R
#' @name quality
NULL



#' @rdname quality
#' @export
setGeneric('Q_local', function(object) standardGeneric('Q_local'),
           valueClass = 'numeric')


#' @export
setMethod('Q_local', 'dimRedResult',
          function(object){
    if(!object@has.org.data) stop('object requires original data')
    if(!requireNamespace('coRanking')) stop('package coRanking required')

    Q <- coRanking::coranking(object@org.data, object@data@data)
    nQ <- nrow(Q)
    N <- nQ + 1
    
    Qnx <- diag(apply(apply(Q, 2, cumsum), 1, cumsum)) / (1:nQ) / N
    lcmc <- Qnx - 1:nQ / nQ

    Kmax <- which.max(lcmc)

    Qlocal <- sum(Qnx[1:Kmax]) / Kmax
    return(Qlocal)
})

#' @rdname quality
#' @export
setGeneric(
    'Q_global', function(object) standardGeneric('Q_global'),
    valueClass = 'numeric'
)

#' @export
setMethod('Q_global', 'dimRedResult',
          function(object){
    if(!object@has.org.data) stop('object requires original data')
    if(!requireNamespace('coRanking')) stop('package coRanking required')

    Q <- coRanking::coranking(object@org.data, object@data@data)
    nQ <- nrow(Q)
    N <- nQ + 1
    
    Qnx <- diag(apply(apply(Q, 2, cumsum), 1, cumsum)) / (1:nQ) / N
    lcmc <- Qnx - 1:nQ / nQ

    Kmax <- which.max(lcmc)

    Qglobal <- sum(Qnx[(Kmax+1):nQ]) / (N - Kmax)
    return(Qglobal)    
})

#' @rdname quality
#' @export
setGeneric(
    'mean_R_NX', function(object) standardGeneric('mean_R_NX'),
    valueClass = 'numeric'
)

#' @export
setMethod('mean_R_NX', 'dimRedResult',
          function(object){
    if(!object@has.org.data) stop('object requires original data')
    if(!requireNamespace('coRanking')) stop('package coRanking required')

    Q <- coRanking::coranking(object@org.data, object@data@data)
    nQ <- nrow(Q)
    N <- nQ + 1
    
    Qnx <- diag(apply(apply(Q, 2, cumsum), 1, cumsum)) / (1:nQ) / N

    ## R_NX is only defined for 1 <= K <= N-2
    Qnx <- Qnx[-length(Qnx)]
    K <- 1:(nQ-1)
    Rnx <- (nQ*Qnx - K) / (nQ-K)
    
    return(mean(Rnx))
})


#' @rdname quality
#' @export
setGeneric(
    'total_correlation',
    function(object, naxes, cor_method, is.rotated) standardGeneric('total_correlation'),
    valueClass = 'numeric'
)



#' @export
setMethod('total_correlation',
          c('dimRedResult',
            'missingORnumeric',
            'missingORcharacter',
            'missingORlogical'),
          function(object, naxes, cor_method, is.rotated){
    if(missing(naxes))      naxes      <- ncol(object@data@data)
    if(missing(cor_method)) cor_method <- 'pearson'
    if(missing(is.rotated)) is.rotated <- FALSE

    if(!object@has.org.data) stop('object requires original data')
    if(length(naxes) != 1 || naxes < 1 || naxes > ncol(object@data@data))
        stop('naxes must specify the numbers of axes to optimize for, ',
             'i.e. a single integer between 1 and ncol(object@data@data)')
    ## try to partially match cor_method:
    cor_methods <- c('pearson', 'kendall', 'spearman')
    cor_method <- cor_methods[pmatch(cor_method, cor_methods)]
    if(is.na(cor_method))
        stop("cor_method must match one of ",
             "'pearson', 'kendall', or 'spearman', ",
             "at least partially.")
    
    if(!is.rotated) {
        rotated_result <- maximize_correlation(
            object, naxes, cor_method
        )
    } else {
        rotated_result <- object
    }

    res <- 0
    for(i in 1:naxes)
        res <- res + mean(correlate(
                         rotated_result@data@data,
                         rotated_result@org.data,
                         cor_method
                     )[i,]^2)
    
    return(res)
})

#' @rdname quality
#' @export
setGeneric('cophenetic_correlation',
           function(object, d, cor_method) standardGeneric('cophenetic_correlation'),
           valueClass = 'numeric')

#' @export
setMethod('cophenetic_correlation',
          c('dimRedResult', 'missingORfunction', 'missingORcharacter'), 
          function(object, d, cor_method){
    if(missing(d)) d <- dist
    if(missing(cor_method)) cor_method <- 'pearson'
    if(!object@has.org.data) stop('object requires original data')
    cor_methods <- c('pearson', 'kendall', 'spearman')
    cor_method <- cor_methods[pmatch(cor_method, cor_methods)]
    if(is.na(cor_method))
        stop("cor_method must match one of ",
             "'pearson', 'kendall', or 'spearman', ",
             "at least partially.")

    d.org <- d(object@org.data)
    d.emb <- d(object@data@data)

    if(!inherits(d.org, 'dist') || !inherits(d.emb, 'dist'))
        stop('d must return a dist object')
    
    res <- correlate(
        d(object@org.data),
        d(object@data@data),
        cor_method
    )
    return(res)
})

#' @rdname quality
#' @export
setGeneric('distance_correlation',
           function(object) standardGeneric('distance_correlation'),
           valueClass = 'numeric')

#' @export
setMethod('distance_correlation',
          'dimRedResult',
          function(object){
    if(!object@has.org.data) stop('object requires original data')
    if(!requireNamespace('energy')) stop('package energy required.')

    energy::dcor(object@org.data, object@data@data)    
})



#' @rdname quality
#' @export
setGeneric('reconstruction_rmse',
           function(object) standardGeneric('reconstruction_rmse'),
           valueClass = 'numeric')

#' @export
setMethod('reconstruction_rmse', 'dimRedResult',
          function(object){
    if(!object@has.org.data) stop('object requires original data')
    if(!object@has.inverse) stop('object requires an inverse function')

    recon <- object@inverse(object@data)

    sqrt(mean((recon@data - object@org.data)^2))
})


dimRedQualityList <- function () {
    return(c('Q_local',
             'Q_global',
             'mean_R_NX',
             'total_correlation',
             'cophenetic_correlation',
             'distance_correlation',
             'reconstruction_rmse'))
}
