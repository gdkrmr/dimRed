#' An S4 class and methods to hold data for dimensionality reduction
#'
#' @slot data of class \code{matrix}, holds the data, observations in
#'     rows, variables in columns
#' @slot meta of class \code{data.frame}, holds meta data such as
#'     classes, internal manifold coordinates, or simply additional
#'     data of the data set. Must have the same number of rows as the
#'     \code{data} slot or be an empty data frame.
#' @param object,x of class dimRedData.
#' @param i a valid index.
#' @import methods
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
#' 
#' @family dimRedData
#' @name dimRedData
#' @aliases dimRedData-class
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

#' @export
setMethod("initialize",
          signature = c("dimRedData"),
          function (.Object, data = matrix(numeric(0), 0, 0), meta = data.frame()) {
    data <- as.matrix(data)
    meta <- as.data.frame(meta)
    .Object <- callNextMethod()
    return(.Object)
})

#' @rdname dimRedData
#' @name as.dimRedData,ANY
setAs(from = 'ANY', to = 'dimRedData',
      def = function(from) new('dimRedData', data = as.matrix(from)))

#' @rdname dimRedData
#' @name as.dimRedData,data.frame
setAs(from = 'data.frame', to = 'dimRedData',
      def = function(from) new('dimRedData', data = as.matrix(from)))


#' @rdname dimRedData
#' @name as,data.frame,dimRedData
setAs(from = 'dimRedData', to = 'data.frame',
      def = function(from) {as.data.frame(from)})

#' @rdname as.data.frame
#' @param row.names unused
#' @param optional unused
#' @param ... unused
setGeneric(
    'as.data.frame',
    function(x, row.names, optional, ...) standardGeneric('as.data.frame'),
    useAsDefault = base::as.data.frame
)

#' Convert to \code{data.frame}
#'
#' Convert a dimRedData object to a data.frame.
#'
#' To avoid column name collisions prefixes for each slot can be given.
#'
#' @param x dimRedResult object.
#' @param meta.prefix prefix for the meta slot
#' @param data.prefix prefix for the dim Red slot
#'
#' @examples
#' as.data.frame(loadDataSet("Iris"), meta.prefix = "")
#'
#' @include misc.R
#' @family dimRedData
#' @method as.data.frame dimRedData
#' @rdname as.data.frame
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


#' @rdname as.dimRedData
#' @param ... parameters
setGeneric(
    'as.dimRedData',
    function(x, ...) standardGeneric('as.dimRedData'),
    valueClass = 'dimRedData'
)

#' Convert to \code{dimRedData}
#'
#' Convert a \code{data.frame} to a dimRedData object using a formula
#'
#' @param x the formula, left hand side is assigned to meta slot
#'     right hand side is assigned to data slot.
#' @param data a data frame
#' @family dimRedData
#' @method as.dimRedData dimRedData
#' @rdname as.dimRedData
#' @export
setMethod(f = 'as.dimRedData',
          signature = c('formula'),
          definition = function(x, data) {
    data <- as.data.frame(data)
    meta <- model.frame(lhs(x), data)
    data <- model.matrix(rhs(x), data)
    return(new("dimRedData", data = data, meta = meta))
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

