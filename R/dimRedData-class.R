#' Class "dimRedData"
#'
#' A class and methods to hold data for dimensionality reduction.
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
#' s3d <- loadDataSet("3D S Curve")
#' as(s3d, "data.frame")
#' iris <- as(iris[,1:4], "dimRedData")
#'
#' getData(s3d)
#' getMeta(s3d)
#' nrow(s3d)
#' s3d[1:40,]
#'
#' @family dimRedData
#' @import methods
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

# #' @rdname dimRedData
# #' @param data coerced into a numeric matrix, goes into slot data
# #' @param meta coerced into a data.frame, goes into slot meta
# #' @export
setMethod("initialize",
          signature = c("dimRedData"),
          function (.Object, data = matrix(numeric(0), 0, 0), meta = data.frame()) {
    data <- as.matrix(data)
    meta <- as.data.frame(meta)
    .Object <- callNextMethod()
    return(.Object)
})

# #' @rdname dimRedData
# #' @name as.dimRedData,ANY               
setAs(from = 'ANY', to = 'dimRedData',
      def = function(from) new('dimRedData', data = as.matrix(from)))

# # #' @rdname dimRedData
# # #' @name as.dimRedData,data.frame
# # setAs(from = 'data.frame', to = 'dimRedData',
# #       def = function(from) new('dimRedData', data = as.matrix(from)))


# #' @rdname dimRedData
# #' @name as,data.frame,dimRedData
setAs(from = 'dimRedData', to = 'data.frame',
      def = function(from) {as.data.frame(from)})

# #' Convert to \code{data.frame}
# #'
# #' Convert a dimRedData or dimRedResult object to a data.frame.
# #'
# #' To avoid column name collisions in the resulting \code{data.frame}
# #' prefixes can be assigned. The parameters \code{row.names},
# #' \code{optional}, and \code{...} are not used.
# #'
# #' @param x dimRedResult/dimRedData object.
# #' @param meta.prefix prefix for the meta data variables.
# #' @param data.prefix for dimRedResult objects: prefix for embedded
# #'     dimensions, for dimRedData objects: prefix for the variable
# #'     names.
# #' @param org.data.prefix for dimRedResult objects: prefix for the
# #'     original variables.
# #' @param row.names unused
# #' @param optional unused
# #' @param ... unused
# #'
# #' @examples
# #' as.data.frame(embed(loadDataSet("Iris"), "PCA"), org.data.prefix = "", meta.prefix = "")


#' @param meta.prefix Prefix for the columns of the meta data names.
#' @param data.prefix Prefix for the columns of the variable names.
#' 
#' @include misc.R
#' @family dimRedData
#' @method as.data.frame dimRedData
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
#' as.dimRedData(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'               iris)
#'
#' @include misc.R
#' @family dimRedData
#' @method as.dimRedData dimRedData
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

#' @param i a valid index.
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

