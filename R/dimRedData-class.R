setClassUnion('missingORnumeric', c('numeric', 'missing'))
setClassUnion('missingORcharacter', c('character', 'missing'))
setClassUnion('missingORlogical', c('logical', 'missing'))
setClassUnion('missingORfunction', c('function', 'missing'))

#' An S4 class and methods to hold data for dimensionality reduction
#'
#' @slot data of class \code{matrix}, holds the data, observations in
#'     rows, variables in columns
#' @slot meta of class \code{data.frame}, holds meta data such as
#'     classes, internal manifold coordinates, or simply additional
#'     data of the data set. Must have the same number of rows as the
#'     \code{data} slot or be an empty data frame.
#'
#' @examples
#' s3d <- loadDataSet("3D S Curve")
#' as(s3d, "data.frame")
#' iris <- as(iris[,1:4], "dimRedData")
#'
#' getData(s3d)
#' getMeta(s3d)
#' nrow(s3d)
#' s3d[1:40,]
#'
#' @name dimRedData
#' @export
dimRedData <- setClass(
    'dimRedData',
    slots     = c(data = 'matrix', meta = 'data.frame'),
    prototype = prototype(data = matrix(numeric(0), 0,0), meta = data.frame()),
    validity  = function (object) {
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
        
        return(if (is.null(retval)) TRUE else retval)
    }
)

#' @rdname dimRedData
#' @name as
setAs(from = 'ANY', to = 'dimRedData',
      def = function(from) new('dimRedData', data = as.matrix(from)))

#' @rdname dimRedData
#' @name as
setAs(from = 'data.frame', to = 'dimRedData',
      def = function(from) new('dimRedData', data = as.matrix(from)))

#' @rdname dimRedData
#' @name as
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

#' @rdname dimRedData
#' @export
setGeneric('getData', function(object) standardGeneric('getData'))

#' @rdname dimRedData
#' @export
setMethod('getData', 'dimRedData', function(object) object@data)

#' @rdname dimRedData
#' @export
setGeneric('getMeta', function(object) standardGeneric('getMeta'))

#' @rdname dimRedData
#' @export
setMethod('getMeta', 'dimRedData', function(object) object@meta)

#' @rdname dimRedData
#' @export
setMethod('nrow', 'dimRedData', function(x) nrow(x@data))

#' @rdname dimRedData
#' @export
setMethod('[', signature(x = 'dimRedData',
                         i = 'ANY'),
          function(x, i) {
    x@data <- x@data[i,,drop=FALSE]
    if(nrow(x@meta) != 0)
        x@meta <- x@meta[i,,drop=FALSE]
    vv <- validObject(x)
    if(vv == TRUE) return(x)
    else stop('cannot subset dimRedData object: \n',
              paste(vv, collapse = '\n'))
}) 

