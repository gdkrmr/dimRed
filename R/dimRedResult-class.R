
#' A class to hold the results of of a dimensionality reduction.
#'
#' @slot data Output data of class dimRedData.
#' @slot org.data original data, a matrix.
#' @slot apply a function to apply the method to out-of-sampledata,
#'     may not exist.
#' @slot inverse a function to calculate the original coordinates from
#'     reduced space, may not exist.
#' @slot has.org.data logical, if the original data is included in the object.
#' @slot has.apply logical, if a forward method is exists.
#' @slot has.inverse logical if an inverse method exists.
#' @slot method saves the method used.
#' @slot pars saves the parameters used.
#'
#' @include dimRed-class.R
#' @export
dimRedResult <- setClass(
    'dimRedResult',
    slots = c(
        data         = 'dimRedData',
        org.data     = 'matrix',
        apply        = 'function',
        inverse      = 'function',
        has.org.data = 'logical',
        has.apply    = 'logical',
        has.inverse  = 'logical',
        method       = 'character',
        pars         = 'list'
    ),
    prototype = list(
        data         = new('dimRedData'),
        org.data     = matrix(numeric(0), 0, 0),
        apply        = function(x) NA,
        inverse      = function(x) NA,
        has.org.data = FALSE,
        has.apply    = FALSE,
        has.inverse  = FALSE,
        method       = "",
        pars         = list()
    )
)



#' @export
setAs(from = 'dimRedResult', to = 'data.frame',
      def = function(from){
          if(from@has.org.data) {
              org.data <- from@org.data
              names(org.data) <- paste("org", names(org.data), sep = ".")
              cbind(as(from@data, 'data.frame'), as.data.frame(org.data))
          } else {
              as.(from@data, 'data.frame')
          }
      })

#' @export
setGeneric('getPars', function (object) standardGeneric('getPars'))

#' @export
setMethod(
    f = 'getPars',
    signature = 'dimRedResult',
    definition = function (object) {
        object@pars
    }
)

#' @export
setMethod(
    f = 'print',
    signature = 'dimRedResult',
    definition = function(x) {
        cat("Method:\n")
        cat(x@method, "\n")
        cat("Parameters:\n")
        str(x@pars)
    }
)

