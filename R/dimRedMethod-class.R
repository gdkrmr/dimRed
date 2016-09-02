
#' An S4 class that makes sure that the wrappers for dimensionality
#' reduction work.
#'
#' @slot fun function call
#'
#' @rdname dimRedMethod
#' 
#' @export
setClass('dimRedMethod',
         slots = c(fun     = 'function',
                   stdpars = 'list'),
         prototype = list(fun     = function() NA,
                          stdpars = list()))


#' @rdname dimRedMethod
#'
#' @export
dimRedMethodList <- function () {
    return(c(
        'graph_kk',
        'graph_drl',
        'graph_fr',
        'drr',
        'isomap',
        'diffmap',
        'tsne',
        'nmds',
        'mds',
        'ica',
        'pca',
        'lle',
        ## those two methods are buggy and can produce segfaults:
        ## "loe", "soe",
        'leim',
        'kpca'
    ))
}


#' to put standard values for omitted arguments
setGeneric('matchPars', function(object, pars) standardGeneric('matchPars'),
           valueClass = c("list"))
setMethod('matchPars',
          signature(object = 'dimRedMethod',
                    pars   = 'list'),
          definition = function(object, pars) {
    nsp <- names(object@stdpars)
    ncp <- names(pars)
    nap <- union(nsp, ncp)

    res <- list()
    
    ## exists can deal with elements being NULL
    ## to assign list@el <- NULL do:
    ## list['el'] <- list(NULL)
    for(np in nap) {
        miss.std <- !exists(np, where = object@stdpars)
        miss.par <- !exists(np, where = pars)
        if (miss.std) {
            warning("Parameter matching: ", np,
                    " is not a standard parameter, ignoring.")
        } else if (miss.par) { 
            res[np] <- object@stdpars[np]
        } else {
            res[np] <- pars[np]
        }
    }

    ## if the method does not accept parameters we have to return
    ## null, so in embed there is no args$par created. and passed by
    ## do.call in the embed() function.  if (length(res) != 0)
    ## return(res) else return(NULL)

    ## first try without the above, all methods should have a pars
    ## argument.
    return(res)
})





