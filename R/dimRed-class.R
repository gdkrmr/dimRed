
#' S4 Class to hold data, method and parameters for dimensionality reduction
#'
#' @slot data holds the original data
#' @slot method holds the method
#' @slot the parameters used with method
#'
#' @include dimRedData-class.R
#' @include dimRedMethod-class.R
#' @include dimRedMethodPars-class.R
#'
#' @export
setClass(
    'dimRed',
    slots = c(
        data = 'dimRedData',
        method = 'dimRedMethod',
        pars = 'dimRedMethodPars'
    ),
    prototype = list(
        data = new('dimRedData'),
        method = new('dimRedMethod'),
        pars = new('dimRedMethodPars')
    )
)

#' Fit a dimred object.
#'
#' Fits a dimred object.
#'
#' the same can be achieved by e.g. \code{fastica@fun(dat, pars = ...)}
#'
#' @param x the dimRed object
#' @export
setGeneric('fit', function(object) standardGeneric('fit'))


#' @export
setMethod(
    f = 'fit',
    signature = 'dimRed',
    definition = function (object) {
        object@method@fun(
            object@data,
            object@pars@pars
        )
    }
)


