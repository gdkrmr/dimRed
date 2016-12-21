## if (!isClassUnion("missingORnumeric"))   setClassUnion("missingORnumeric",   c("numeric", "missing"))
## if (!isClassUnion("missingORcharacter")) setClassUnion("missingORcharacter", c("character", "missing"))
## if (!isClassUnion("missingORlogical"))   setClassUnion("missingORlogical",   c("logical", "missing"))
## if (!isClassUnion("missingORfunction"))  setClassUnion("missingORfunction",  c("function", "missing"))

# Squared euclidean distance between points in A and B
# taken from http://blog.felixriedel.com/2013/05/pairwise-distances-in-r/

pdist2 <- function (A, B) {
    an <- rowSums(A ^ 2) # apply(A, 1, function(rvec) crossprod(rvec, rvec))
    bn <- rowSums(B ^ 2) # apply(B, 1, function(rvec) crossprod(rvec, rvec))

    m <- nrow(A)
    n <- nrow(B)

    matrix(rep(an, n), nrow = m) +
        matrix(rep(bn, m), nrow = m, byrow = TRUE) -
        2 * tcrossprod(A, B)
}


## a + b ~ c + d
## becomes
## ~ c + d + 0
rhs <- function (formula) {
    fs <- as.character(formula)[3]
    stats::as.formula(paste("~", fs, "+ 0"))
}

## a + b ~ c + d
## becomes
## ~ a + b + 0
lhs <- function (formula) {
    fs <- as.character(formula)[2]
    stats::as.formula(paste("~", fs, "+ 0"))
}

## check if a package is installed
chckpkg <- function (pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(paste0("require '", pkg,
                    "' package, install it using install.packages('",
                    pkg, "')"))
    }
}

## create generics that appear in several different places

#' Converts to data.frame
#'
#' General conversions of objects created by \code{dimRed} to
#' \code{data.frame}. See class documentations for details. For the
#' original documentation of this function see here:
#' \code{\link[base]{as.data.frame.default}}.
#'
#' @param x The object to be converted
#' @param row.names unused in \code{dimRed}
#' @param optional unused in \code{dimRed}
#' @param ... other arguments.
setGeneric(
    "as.data.frame",
    function(x, row.names, optional, ...) standardGeneric("as.data.frame"),
    useAsDefault = base::as.data.frame,
    valueClass = "data.frame"
)

#' Converts to dimRedData
#'
#' Conversion functions to dimRedData.
#'
#' @param formula a formula object.
#' @param ... other arguments.
setGeneric(
    "as.dimRedData",
    function(formula, ...) standardGeneric("as.dimRedData"),
    valueClass = "dimRedData"
)

#' Method getData
#'
#' Extracts the data slot.
#'
#' @param object The object to be converted.
setGeneric("getData", function(object) standardGeneric("getData"))

#' Method getMeta
#'
#' Extracts the meta slot.
#'
#' @param object The object to be converted.
#' @param ... other arguments.
setGeneric("getMeta", function(object, ...) standardGeneric("getMeta"))

#' Method getPars
#'
#' Extracts the pars slot.
#'
#' @param object The object to be converted.
#' @param ... other arguments.
setGeneric("getPars", function (object, ...) standardGeneric("getPars"))

#' Method getOrgData
#'
#' Extract the Original data.
#'
#' @param object The object to extract data from.
#' @param ... other arguments.
setGeneric("getOrgData", function (object, ...) standardGeneric("getOrgData"))

#' Method getDimRedData
#'
#' Extract dimRedData.
#' @param object The object to extract data from.
#' @param ... other arguments.
setGeneric("getDimRedData",
           function (object, ...) standardGeneric("getDimRedData"))

#' Method print
#'
#' Imports the print method into the package namespace.
#'
#' @param x The object to be printed.
#' @param ... Other arguments for printing.
setGeneric("print", function(x, ...) standardGeneric("print"))


#' Method ndims
#'
#' Extract the number of dimensions.
#'
#' @param object To extract the number of dimensions from.
#' @param ... Arguments for further methods
setGeneric("ndims",
           function (object, ...) standardGeneric("ndims"),
           valueClass = "integer")


#' getSuggests
#'
#' Install packages wich are suggested by dimRed.
#'
#' By default dimRed will not install all the dependencies, because
#' there are quite a lot and in case some of them are not available
#' for your platform you will not be able to install dimRed without
#' problems.
#'
#' To solve this I provide a function which automatically installes
#' all the suggested packages.
#'
#' @examples
#' \dontrun{
#' installSuggests()
#' }
#' @export
installSuggests <- function () {
    "%w/o%" <- function(x, y) x[!x %in% y]
    pkgString <- installed.packages()["dimRed", "Suggests"]
    deps <- strsplit(pkgString, ", |,\n")[[1]]
    deps <- gsub("\n", "", deps)        # Windows needs this

    installedPkgs <- rownames(installed.packages())
    missingPkgs <- deps %w/o% installedPkgs

    if (length(missingPkgs) > 0) {
        message("The following packages are missing: ")
        cat(missingPkgs, "\n")
        message("installing ...")
        install.packages(missingPkgs)
        pkgString <- installed.packages()["dimRed", "Suggests"]
        installedPkgs <- rownames(installed.packages())
        missingPkgs <- deps %w/o% installedPkgs
        if (length(missingPkgs) > 0) {
            message("Could not install the following packages:")
            cat(missingPkgs, "\n")
            message("please install manually or some methods will not work.")
        } else {
            message("All necessary packages installed")
            message("If things still don't work try 'update.package()'")
            message("If it still does not work file a bugreport!!")
        }
    } else {
        message("All necessary packages installed")
        message("If things still don't work try 'update.package()'")
        message("If it still does not work file a bugreport!!")
    }

}


## input data(matrix or data frame) return knn graph implements
## "smart" choices on RANN::nn2 parameters we ignore radius search
## TODO: find out a good limit to switch from kd to bd trees COMMENT:
## bd trees are buggy, they dont work if there are duplicated data
## points and checking would neutralize the performance gain, so bd
## trees are not really usable.

#' makeKNNgraph
#'
#' Create a K-nearest neighbor graph from data x. Uses
#' \code{\link[RANN]{nn2}} as a fast way to find the neares neighbors.
#'
#' @param x data, a matrix, observations in rows, dimensions in
#'     columns
#' @param k the number of nearest neighbors.
#' @param eps number, if \code{eps > 0} the KNN search is approximate,
#'     see \code{\link[RANN]{nn2}}
#' @param diag logical, if \code{TRUE} every edge of the returned
#'     graph will have an edge with weight \code{0} to itself.
#'
#' @return an object of type \code{\link[igraph]{igraph}} with edge
#'     weight being the distances.
#' 
#' 
#' 
makeKNNgraph <- function(x, k, eps = 0, diag = FALSE){
    ## requireNamespace("RANN")
    ## requireNamespace("igraph")

    ## consts
    INF_VAL <- 1.340781e+15
    NA_IDX  <- 0
    BDKD_LIM <- 1000000                 #todo: figure out a good value here

    ## select parameters
    M <- nrow(x)
    treetype <- "kd"                # if (M < BDKD_LIM) "kd" else "bd"
                                    # see:
                                    # https://github.com/jefferis/RANN/issues/19
    searchtype <- if (eps == 0) "standard" else "priority"

    ## RANN::nn2 returns the points in data with respect to query
    ## e.g. the rows in the output are the points in query and the
    ## columns the points in data.
    nn2res <- RANN::nn2(data = x, query = x, k = k + 1, treetype = treetype,
                        searchtype = searchtype, eps = eps)

    ## create graph: the first ny nodes will be y, the last nx nodes
    ## will be x, if x != y
    g <- igraph::make_empty_graph(M, directed = FALSE)
    g[from = if (diag) rep(seq_len(M), times = k + 1)
             else      rep(seq_len(M), times = k),
      to   = if (diag) as.vector(nn2res$nn.idx)
             else      as.vector(nn2res$nn.idx[, -1]),
      attr = "weight"] <- if (diag) as.vector(nn2res$nn.dists)
                          else      as.vector(nn2res$nn.dists[, -1])

    return(g)
}


makeEpsSparseMatrix <- function(x, eps) {
    chckpkg("Matrix")
    n <- nrow(x)
    dd <- dist(x)
    ddind <- dd < eps
    rows <- unlist(lapply(2:n, function(x) x:n), use.names = FALSE)
    cols <- rep(seq_len(n - 1), times = (n - 1):1)
    sparseMatrix(i = rows[ddind],
                 j = cols[ddind],
                 x = dd[ddind],
                 dims = c(n, n),
                 symmetric = TRUE)
}
