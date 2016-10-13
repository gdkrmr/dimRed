#' @include misc.R
NULL


#' Class "dimRedData"
#'
#' A class to hold data for dimensionality reduction and methods.
#'
#' The class hast two slots, \code{data} and \code{meta}. The
#' \code{data} slot contains a \code{numeric matrix} with variables in
#' columns and observations in rows. The \code{meta} slot may contain
#' a \code{data.frame} with additional information. Both slots need to
#' have the same number of rows or the \code{meta} slot needs to
#' contain an empty \code{data.frame}.
#'
#' See examples for easy conversion from and to \code{data.frame}.
#'
#' For plotting functions see \code{\link{plot.dimRedData}}.
#'
#' @slot data of class \code{matrix}, holds the data, observations in
#'     rows, variables in columns
#' @slot meta of class \code{data.frame}, holds meta data such as
#'     classes, internal manifold coordinates, or simply additional
#'     data of the data set. Must have the same number of rows as the
#'     \code{data} slot or be an empty data frame.
#'
#' 
#' @examples
#' ## Load an example data set:
#' s3d <- loadDataSet("3D S Curve")
#'
#' ## Create using a constructor:
#' 
#' ### without meta information:
#' dimRedData(iris[,1:4])
#' 
#' ### with meta information:
#' dimRedData(iris[,1:4], iris[,5])
#'
#' ### using slot names:
#' dimRedData(data = iris[,1:4], meta = iris[,5])
#' 
#' ## Convert to a dimRedData objects:
#' Iris <- as(iris[,1:4], "dimRedData")
#'
#' ## Convert to data.frame:
#' head(as(s3d, "data.frame"))
#' head(as.data.frame(s3d))
#'
#' ## Extract slots:
#' head(getData(s3d))
#' head(getMeta(s3d))
#'
#' ## Get the number of observations:
#' nrow(s3d)
#'
#' ## Subset:
#' s3d[1:5,]
#'
#' @family dimRedData
#' @import methods
#' @export dimRedData
#' @exportClass dimRedData
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

setMethod("initialize",
          signature = c("dimRedData"),
          function (.Object, data = matrix(numeric(0), 0, 0), meta = data.frame()) {
    data <- as.matrix(data)
    meta <- as.data.frame(meta)
    .Object <- callNextMethod()
    return(.Object)
})

            
setAs(from = 'ANY', to = 'dimRedData',
      def = function(from) new('dimRedData', data = as.matrix(from)))

setAs(from = 'dimRedData', to = 'data.frame',
      def = function(from) {as.data.frame(from)})

#' @param meta.prefix Prefix for the columns of the meta data names.
#' @param data.prefix Prefix for the columns of the variable names.
#' 
#' @family dimRedData
#' @describeIn dimRedData convert to data.frame
#' @export
setMethod(f = 'as.data.frame',
          signature = c('dimRedData'),
          definition = function(x, meta.prefix = "meta.",
                                data.prefix = "") {
    res <- cbind(as.data.frame(x@meta),
                 as.data.frame(x@data))
    names(res) <- c(paste0(meta.prefix, colnames(x@meta)),
                    paste0(data.prefix, colnames(x@data)))
    return(res)
})


#' @param formula The formula, left hand side is assigned to the meta slot
#'     right hand side is assigned to the data slot.
#' @param data A data frame
#' 
#' @examples
#' ## create a dimRedData object using a formula
#' as.dimRedData(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'               iris)[1:5]
#'
#' @include misc.R
#' @family dimRedData
#' @describeIn dimRedData Convert a \code{data.frame} to a dimRedData
#'     object using a formula
#' @export
setMethod(f = 'as.dimRedData',
          signature = c('formula'),
          definition = function(formula, data) {
    data <- as.data.frame(data)
    meta <- stats::model.frame(lhs(formula), data)
    data <- stats::model.matrix(rhs(formula), data)
    return(new("dimRedData", data = data, meta = meta))
})



#' @param object Of class dimRedData.
#' @describeIn dimRedData Get the data slot.
#' @export
setMethod('getData', 'dimRedData', function(object) object@data)


#' @describeIn dimRedData Get the meta slot.
#' @export
setMethod('getMeta', 'dimRedData', function(object) object@meta)

#' @param x Of class dimRedData
#' @describeIn dimRedData Get the number of observations.
#' @export
setMethod('nrow', 'dimRedData', function(x) nrow(x@data))

#' @param i a valid index for subsetting rows.
#' @examples
#' ## Shuffle data:
#' s3 <- s3d[nrow(s3d)]
#' 
#' @describeIn dimRedData Subset rows.
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


#' @describeIn dimRedData Extract the number of Variables from the data.
#' 
#' @examples
#' ## Get the number of variables:
#' ndims(s3d)
#'
#' @export
setMethod('ndims', 'dimRedData', function(object) ncol(object@data))
