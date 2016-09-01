

#' An S4 class to hold data for dimensionality reduction
#'
#' @slot data of class \code{matrix}, holds the data, observations in
#'     rows, variables in columns
#' @slot meta of class \code{data.frame}, holds meta data such as
#'     classes, internal manifold coordinates, or simply additional
#'     data of the data set. Must have the same number of rows as the
#'     \code{data} slot or be an empty data frame.
#'
#' @examples
#'
#' 
#'
#' @export
setClass(
    'dimRedData',
    slots = c(data = 'matrix', meta = 'data.frame'),
    prototype = prototype(data = matrix(numeric(0), 0,0), meta = data.frame())
)

#' @export
setValidity(
    'dimRedData',
    function (object) {
        retval <- NULL
        if(!is.matrix(object@data)) {
            retval <- c(
                retval,
                c('data must be a matrix with observations in rows and dimensions in columns')
            )
        }
        if(!is.numeric(object@data)) {
            retval <- c(
                retval,
                c('data must be numeric')
            )
        }
        if((nrow(object@meta) != 0) && (nrow(object@meta) != nrow(object@data))){
            retval <- c(
                retval,
                c('data and meta must have the same numbers of rows')
            )
        }
        
        if (is.null(retval)) {
            return(TRUE)
        } else {
            return(retval)
        }
    }
)

#' @export
setAs(from = 'ANY', to = 'dimRedData',
      def = function(from) new('dimRedData', data = as.matrix(from)))
#' @export
setAs(from = 'data.frame', to = 'dimRedData',
      def = function(from) new('dimRedData', data = as.matrix(from)))
#' @export
setAs(from = 'dimRedData', to = 'data.frame',
      def = function(from) {
          res <- cbind(from@meta, as.data.frame(from@data))
          resnames <- c(
              paste("meta", colnames(from@meta), sep = '.'),
              colnames(from@data)
          )
          names(res) <- resnames
          return(res)
      })

#' @export
setGeneric('getData', function(object) standardGeneric('getData'))
#' @export
setMethod('getData', 'dimRedData', function(object) object@data)

#' @export
setGeneric('getMeta', function(object) standardGeneric('getMeta'))
#' @export
setMethod('getMeta', 'dimRedData', function(object) object@meta)

#' @export
setMethod('nrow', 'dimRedData', function(x) nrow(x@data))

#' @export
setMethod('[', signature(x = 'dimRedData',
                         i = 'ANY'),
          function(x, i) {
    x@data <- x@data[i,]
    if(nrow(x@meta) != 0)
        x@meta <- x@meta[i,]
    vv <- validObject(x)
    if(vv == TRUE) return(x)
    else stop('cannot subset dimRedData object: \n',
              paste(vv, collapse = '\n'))
}) 

