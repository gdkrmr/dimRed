

#' dispatches the different methods for dimensionality reduction
#'
#' wraps around all dimensionality reduction functions.
#'
#' Method must be one of \code{dimRedMethodList()}, partial matching
#' is performed.
#'
#' @param data object of class \code{dimRedData}
#' @param method character vector naming one of the dimensionality
#'     reduction techniques
#' @param ... the pameters, internally passed as a list to the
#'     dimensionality reduction method as \code{pars = list(...)}
#' @return an object of class \code{dimRedResult}
#'
#' @examples
#' embed_methods <- dimRedMethodList() 
#' quality_methods <- dimRedQualityList()
#' scurve <- loadDataSet("3D S Curve", n = 500)
#'
#' quality_results <- matrix(NA, length(embed_methods), length(quality_methods),
#'                               dimnames = list(embed_methods, quality_methods))
#' embedded_data <- list()
#' 
#' for(e in embed_methods) {
#'   message("embedding: ", e)
#'   embedded_data[[e]] <- embed(scurve, e)
#'   for(q in quality_methods) {
#'     message("  quality: ", q)
#'     try(quality_results[e,q] <- quality(embedded_data[[e]], q))
#'   }
#' }
#'
#' print(quality_results)
#' 
#' 
#' 
#' @export
embed <- function(data, method = dimRedMethodList(), keep.org.data = TRUE, ...){
    method <- match.arg(method)
    
    methodObject <- getMethodObject(method)
    
    args <- list(
        data          = data,
        keep.org.data = keep.org.data
    )
    args$pars          = matchPars(methodObject, list(...))

    do.call(methodObject@fun, args)
}


getMethodObject <- function (method) {
    switch(
        method,
        graph_kk  = kamada_kawai,
        graph_drl = drl,
        graph_fr  = fruchterman_reingold,
        drr       = drr,
        isomap    = isomap,
        diffmap   = diffmap,
        tsne      = tsne,
        nmds      = nmds,
        mds       = mds,
        ica       = fastica,
        pca       = pca,
        lle       = lle,
        loe       = loe,
        soe       = soe,
        leim      = leim,
        kpca      = kpca
    )
}

#' dispatches the different methods for quality assessment
#'
#' wraps around all quality assessment functions functions.
#'
#' method must be one of \code{"\link{Q_local}", "\link{Q_global}",
#' "\link{mean_R_NX}", "\link{total_correlation}",
#' "\link{cophenetic_correlation}", "\link{distance_correlation}",
#' "\link{reconstruction_rmse}"}
#'
#' @param data object of class \code{dimRedResult}
#' @param method character vector naming one of the methods
#' @param ... the pameters, internally passed as a list to the
#'     quality method as \code{pars = list(...)}
#' @return a number
#'
#' @examples
#' embed_methods <- c("graph_kk", "graph_drl", "graph_fr", "isomap", "tsne", "nmds",
#'                    "mds", "ica", "pca", "lle", "drr",
#'                    #"loe", "soe",
#'                    "leim", "kpca")
#' quality_methods <- c("Q_local", "Q_global", "mean_R_NX", "total_correlation",
#'                      "cophenetic_correlation", "distance_correlation",
#'                      "reconstruction_rmse")
#' scurve <- loadDataSet("3D S Curve", n = 500)
#'
#' quality_results <- matrix(NA, length(embed_methods), length(quality_methods),
#'                               dimnames = list(embed_methods, quality_methods))
#' embedded_data <- list()
#' 
#' for(e in embed_methods) {
#'   message("embedding: ", e)
#'   embedded_data[[e]] <- embed(scurve, e)
#'   for(q in quality_methods) {
#'     message("  quality: ", q)
#'     try(quality_results[e,q] <- quality(embedded_data[[e]], q))
#'   }
#' }
#'
#' print(quality_results)
#' 
#' @export
quality <- function(data, method = dimRedQualityList(), ...){

    method <- match.arg(method)
    
    args = list(object = data)
    pars = list(...)
    if(length(pars) != 0) args$pars <- pars
    
    switch(
        method,
        Q_local                = do.call( Q_local,                args),
        Q_global               = do.call( Q_global,               args),
        mean_R_NX              = do.call( mean_R_NX,              args),
        total_correlation      = do.call( total_correlation,      args),
        cophenetic_correlation = do.call( cophenetic_correlation, args),
        distance_correlation   = do.call( distance_correlation,   args),
        reconstruction_rmse    = do.call( reconstruction_rmse,    args)
    )
}

