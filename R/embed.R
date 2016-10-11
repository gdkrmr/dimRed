#' dispatches the different methods for dimensionality reduction
#'
#' wraps around all dimensionality reduction functions.
#'
#' Method must be one of \code{dimRedMethodList()}, partial matching
#' is performed. All parameters start with a dot, to avoid clashes
#' with partial argument matching (see the R manual section 4.3.2), if
#' there should ever occur any clashes in the arguments, call the
#' function with all arguments named \code{embed(.data = dat, .method
#' = "PCA", .d = "something")}.
#'
#' @param .data object of class \code{dimRedData}
#' @param .method character vector naming one of the dimensionality
#'     reduction techniques.
#' @param .mute turn off printing of warnings/errors/messages of
#'     wrapped functions.
#' @param .keep.org.data TRUE/FALSE keep the original data.
#' @param ... the pameters, internally passed as a list to the
#'     dimensionality reduction method as \code{pars = list(...)}
#' @return an object of class \code{dimRedResult}
#'
#' @examples
#' embed_methods <- dimRedMethodList() 
#' quality_methods <- dimRedQualityList()
#' dataset <- loadDataSet("Iris")
#'
#' quality_results <- matrix(NA, length(embed_methods), length(quality_methods),
#'                               dimnames = list(embed_methods, quality_methods))
#' embedded_data <- list()
#' 
#' for(e in embed_methods) {
#'   message("embedding: ", e)
#'   embedded_data[[e]] <- embed(dataset, e)
#'   for(q in quality_methods) {
#'     message("  quality: ", q)
#'     quality_results[e,q] <- tryCatch(
#'       quality(embedded_data[[e]], q),
#'       error = function(e) NA
#'     )
#'   }
#' }
#'
#' print(quality_results)
#' 
#' 
#' 
#' @export
embed <- function(.data, .method = dimRedMethodList(),
                  .mute = c('message', 'output'),
                  .keep.org.data = TRUE,
                  ...){
    method <- match.arg(.method)
    
    methodObject <- getMethodObject(method)
    
    args <- list(
        data          = as(.data, "dimRedData"),
        keep.org.data = .keep.org.data
    )
    args$pars <- matchPars(methodObject, list(...))
    
    devnull <- if(Sys.info()['sysname'] != "Windows") "/dev/null" else "NUL"
    if('message' %in% .mute){
        devnull1 <- file(devnull,  'wt')
        sink(devnull1, type = 'message')
        on.exit({
            sink(file = NULL, type = "message")
            close(devnull1)
        }, add = TRUE)
    }
    if('output' %in% .mute) {
        devnull2 <- file(devnull,  'wt')
        sink(devnull2, type = 'output')
        on.exit({
            sink()
            close(devnull2)
        }, add = TRUE)
    }
        
    do.call(methodObject@fun, args)
}


getMethodObject <- function (method) {
    ## switch(
    ##     method,
    ##     graph_kk  = kamada_kawai,
    ##     graph_drl = drl,
    ##     graph_fr  = fruchterman_reingold,
    ##     drr       = drr,
    ##     isomap    = isomap,
    ##     diffmap   = diffmap,
    ##     tsne      = tsne,
    ##     nmds      = nmds,
    ##     mds       = mds,
    ##     ica       = fastica,
    ##     pca       = pca,
    ##     lle       = lle,
    ##     loe       = loe,
    ##     soe       = soe,
    ##     leim      = leim,
    ##     kpca      = kpca
    ## )
    method <- match.arg(method, dimRedMethodList())
    do.call(method, list())
}

