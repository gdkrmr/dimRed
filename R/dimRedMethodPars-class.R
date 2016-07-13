
#' S4 Class to store method parameters
#'
#' at the moment nothing more than a wrapper around a list
#'
#' @slot pars a named list of parameters
#'  
#' @export
setClass(
    'dimRedMethodPars',
    slots = c(
        pars = 'list'
    ),
    prototype = prototype(
        pars = list()
    )
)

