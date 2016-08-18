

#' dispatches the different methods for dimensionality reduction
#'
#' wraps around all dimensionality reduction functions.
#'
#' method must be one of \code{"\link{graph_kk}",
#' "\link{graph_drl}", "\link{graph_fr}",
#' "\link{isomap}", "\link{tsne}", "\link{nmds}",
#' "\link{mds}", "\link{ica}", "\link{pca}",
#' "\link{lle}", "\link{loe}", "\link{soe}",
#' "\link{leim}", "\link{kpca}"}
#'
#' @param data object of class \code{dimRedData}
#' @param method character vector naming one of the dimensionality
#'     reduction techniques
#' @param ... the pameters, internally passed as a list to the
#'     dimensionality reduction method as \code{pars = list(...)}
#' @return an object of class \code{dimRedResult}
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
#' 
#' 
#' @export
embed <- function(data, method, keep.org.data = TRUE, ...){

    args <- list(data = data, keep.org.data = keep.org.data)

    pars <- list(...)
    if(length(pars) != 0) args$pars <- pars
    
    switch(
        method,
        graph_kk  = do.call( kamada_kawai@fun,         args ),
        graph_drl = do.call( drl@fun,                  args ),
        graph_fr  = do.call( fruchterman_reingold@fun, args ),
        drr       = do.call( drr@fun,                  args ),
        isomap    = do.call( isomap@fun,               args ),
        tsne      = do.call( tsne@fun,                 args ),
        nmds      = do.call( nmds@fun,                 args ),
        mds       = do.call( mds@fun,                  args ),
        ica       = do.call( fastica@fun,              args ),
        pca       = do.call( pca@fun,                  args ),
        lle       = do.call( lle@fun,                  args ),
        loe       = do.call( loe@fun,                  args ),
        soe       = do.call( soe@fun,                  args ),
        leim      = do.call( leim@fun,                 args ),
        kpca      = do.call( kpca@fun,                 args ),
        stop("method ", method, " unknown")
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
quality <- function(data, method, ...){

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
        reconstruction_rmse    = do.call( reconstruction_rmse,    args),
        stop("method ", method, " unknown")        
    )
    
}

