#' @include misc.R
#' @include dimRedData-class.R
NULL

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
#' @examples
#' ## Create object by embedding data
#' iris.pca <- embed(loadDataSet("Iris"), "PCA")
#'
#' ## Convert the result to a data.frame
#' head(as(iris.pca, "data.frame"))
#' head(as.data.frame(iris.pca))
#' 
#' ## There are no nameclashes to avoid here:
#' head(as.data.frame(iris.pca,
#'                    org.data.prefix = "",
#'                    meta.prefix     = "",
#'                    data.prefix     = ""))
#'
#' ## Print it more or less nicely:
#' print(iris.pca)
#'
#' ## Get the embedded data as a dimRedData object:
#' getDimRedData(iris.pca)
#'
#' ## Get the original data including meta information:
#' getOrgData(iris.pca)
#'
#' @family dimRedResult
#' @export dimRedResult
#' @exportClass dimRedResult
dimRedResult <- setClass(
    "dimRedResult",
    slots = c(
        data         = "dimRedData",
        org.data     = "matrix",
        apply        = "function",
        inverse      = "function",
        has.org.data = "logical",
        has.apply    = "logical",
        has.inverse  = "logical",
        method       = "character",
        pars         = "list"
    ),
    prototype = list(
        data         = new("dimRedData"),
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

setAs(
    from = "dimRedResult",
    to = "data.frame",
    def = function(from){
        if (from@has.org.data) {
            org.data <- from@org.data
            names(org.data) <- paste("org", names(org.data), sep = ".")
            cbind(as(from@data, "data.frame"), as.data.frame(org.data))
        } else {
            as(from@data, "data.frame")
        }
    }
)


#' @param x Of class \code{dimRedResult}
#' @param org.data.prefix Prefix for the columns of the org.data slot.
#' @param meta.prefix Prefix for the columns of \code{x@@data@@meta}.
#' @param data.prefix Prefix for the columns of \code{x@@data@@data}.
#'
#' @describeIn dimRedResult convert to \code{data.frame}
#' @export
setMethod(f = "as.data.frame",
          signature = c("dimRedResult"),
          definition = function(x, org.data.prefix = "org.",
                                meta.prefix = "meta.",
                                data.prefix = "") {
    tmp <- list()
    
    if (nrow(x@data@meta) > 0){
        tmp$meta <- as.data.frame(x@data@meta)
        names(tmp$meta) <- paste0(meta.prefix,
                                  colnames(x@data@meta))
    }
    tmp$data <- as.data.frame(x@data@data)
    names(tmp$data) <- paste0(data.prefix,   colnames(x@data@data))
    if (x@has.org.data){
        tmp$org.data <- as.data.frame(x@org.data)
        names(tmp$org.data) <- paste0(org.data.prefix, colnames(x@org.data))
    }
    names(tmp) <- NULL
    data.frame(tmp, stringsAsFactors = FALSE)
})



#' @param object Of class \code{dimRedResult}
#' @describeIn dimRedResult Get the parameters with which the method
#'     was called.
#' @export
setMethod(
    f = "getPars",
    signature = "dimRedResult",
    definition = function (object) {
        object@pars
    }
)


#' @describeIn dimRedResult Method for printing.
#' @import utils
#' @export
setMethod(
    f = "print",
    signature = "dimRedResult",
    definition = function(x) {
        cat("Method:\n")
        cat(x@method, "\n")
        cat("Parameters:\n")
        utils::str(x@pars)
    }
)

#' @describeIn dimRedResult Get the original data and meta.data
#' @export
setMethod(
    f = "getOrgData",
    signature = "dimRedResult",
    definition = function(object) {
        return(new("dimRedData",
                   data = object@org.data,
                   meta = object@data@meta))
    }
)

#' @describeIn dimRedResult Get the embedded data
#' @export
setMethod(
    f = "getDimRedData",
    signature = "dimRedResult",
    definition = function(object) {
        return(object@data)
    }
)

#' @describeIn dimRedResult Extract the number of embedding dimensions.
#' 
#' @examples
#' ## Get the number of variables:
#' ndims(iris.pca)
#'
#' @export
setMethod(
    "ndims",
    "dimRedResult",
    function(object) ncol(object@data@data)
)
