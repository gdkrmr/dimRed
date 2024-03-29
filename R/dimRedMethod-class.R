#' Class "dimRedMethod"
#'
#' A virtual class "dimRedMethod" to serve as a template to implement
#' methods for dimensionality reduction.
#'
#' Implementations of dimensionality reductions should inherit from
#' this class.
#'
#' The \code{fun} slot should be a function that takes three arguments
#' \describe{
#'   \item{data}{An object of class \code{\link{dimRedData}}.}
#'   \item{pars}{A list with the standard parameters.}
#'   \item{keep.org.data}{Logical. If the original data should be kept in the output.}
#' }
#' and returns an object of class \code{\link{dimRedResult}}.
#'
#' The \code{stdpars} slot should take a list that contains standard
#' parameters for the implemented methods.
#'
#' This way the method can be called by \code{embed(data, "method-name",
#' ...)}, where \code{...} can be used to to change single parameters.
#'
#'
#' @slot fun A function that does the embedding.
#' @slot stdpars A list with the default parameters for the \code{fun} slot.
#' @slot requires A vector with all packages R packages that need to be
#'   installed to run the method. In some occasions a method may work without
#'   one of the packages. Does not include Python dependencies such as
#'   Tensorflow. Used to auto skip tests
#'
#' @family dimensionality reduction methods
#' @export
setClass("dimRedMethod",
         contains  = "VIRTUAL",
         slots     = c(fun     = "function",
                       stdpars = "list",
                       requires = "character"))


#' dimRedMethodList
#'
#' Get the names of all methods for dimensionality reduction.
#'
#' Returns the name of all classes that inherit from
#' \code{\link{dimRedMethod-class}} to use with \code{\link{embed}}.
#' @param filter filter methods by methods that have their dependencies installed
#' @return a character vector with the names of classes that inherit
#'     from \code{dimRedMethod}.
#'
#' @examples
#' dimRedMethodList()
#'
#' @family dimensionality reduction methods
#' @export
dimRedMethodList <- function (filter = FALSE) {
   all_methods <- names(completeClassDefinition("dimRedMethod", doExtends = FALSE)@subclasses)
   if(!filter)
     return(all_methods)
   all_deps <- lapply(all_methods, getMethodDependencies)
   is_possible <- sapply(all_deps, function(x) {
     if(length(x) > 0)
       requireNamespace(x, quietly = TRUE)
     else
       TRUE
   })
   all_methods[is_possible]
}


# to put standard values for omitted arguments

setGeneric("matchPars", function(object, pars) standardGeneric("matchPars"),
           valueClass = c("list"))


setMethod("matchPars",
          signature(object = "dimRedMethod",
                    pars   = "list"),
          definition = function(object, pars) {
    nsp <- names(object@stdpars)
    ncp <- names(pars)
    nap <- union(nsp, ncp)

    res <- list()

    ## exists can deal with elements being NULL
    ## to assign list@el <- NULL do:
    ## list["el"] <- list(NULL)
    for (np in nap) {
        miss.std <- !exists(np, where = object@stdpars)
        miss.par <- !exists(np, where = pars)
        if (miss.std) {
            warning("Parameter matching: ", np,
                    " is not a standard parameter, ignoring.")
        } else if (miss.par) {
            res[np] <- object@stdpars[np]
        } else {
            res[np] <- pars[np]
        }
    }

    ## if the method does not accept parameters we have to return
    ## null, so in embed there is no args$par created. and passed by
    ## do.call in the embed() function.  if (length(res) != 0)
    ## return(res) else return(NULL)

    ## first try without the above, all methods should have a pars
    ## argument.
    return(res)
})

getMethodDependencies <- function(method) {
  getMethodObject(method)@requires
}

method_can_run <- function(method) {
  all(getMethodDependencies(method) %in% row.names(installed.packages()))
}
