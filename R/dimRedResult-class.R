
#' Class "dimRedResult"
#'
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
#' @include dimRedData-class.R
#' @family dimRedResult
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



# #' As("dimRedResult", "data.frame")
# #' 
# #' @name as
setAs(from = 'dimRedResult', to = 'data.frame',
      def = function(from){
          if(from@has.org.data) {
              org.data <- from@org.data
              names(org.data) <- paste("org", names(org.data), sep = ".")
              cbind(as(from@data, 'data.frame'), as.data.frame(org.data))
          } else {
              as(from@data, 'data.frame')
          }
      })




#' @param x Of class \code{dimRedResult}
#' @param org.data.prefix Prefix for the columns of the org.data slot.
#' @param meta.prefix Prefix for the columns of \code{x@@data@@meta}.
#' @param data.prefix Prefix for the columns of \code{x@@data@@data}.
#' @method as.data.frame dimRedResult
#' @include dimRedData-class.R
#' @describeIn dimRedResult convert to \code{data.frame}
#' @export
setMethod(f = 'as.data.frame',
          signature = c('dimRedResult'),
          definition = function(x, org.data.prefix = "org.",
                                meta.prefix = "meta.",
                                data.prefix = "") {
    cbind(
        as.data.frame(
            x@data@meta,
            col.names     = paste0(org.data.prefix, colnames(x@data@meta))),
        as.data.frame(
            x@data@data,
            col.names     = paste0(data.prefix,   colnames(x@data@data))),
        if(x@has.org.data)
            as.data.frame(
                x@org.data,
                col.names = paste0(org.data.prefix, colnames(x@org.data)))
    )
})



#' @param object Of class \code{dimRedResult}
#' @describeIn dimRedResult Get the parameters with which the method
#'     was called.
#' @export
setMethod(
    f = 'getPars',
    signature = 'dimRedResult',
    definition = function (object) {
        object@pars
    }
)


#' @describeIn dimRedResult Method for printing.
#' @import utils
#' @export
setMethod(
    f = 'print',
    signature = 'dimRedResult',
    definition = function(x) {
        cat("Method:\n")
        cat(x@method, "\n")
        cat("Parameters:\n")
        utils::str(x@pars)
    }
)

