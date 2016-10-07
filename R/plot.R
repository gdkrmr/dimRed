#' Plotting of dimRed* objects
#'
#' Plots a object of class dimRedResult and dimRedData
#'
#' somewhat inflexible, if you require custom colors or want to modify
#' graphical parameters, write your own plotting function.
#'
#' @param x dimRedResult/dimRedData class, e.g. output of
#'     embedded/loadDataSet
#' @param y Ignored
#' @param type plot type, one of \code{c("pairs", "parallel", "2d",
#'     "3d")}
#' @param col the columns of the meta slot to use for coloring, can be
#'     referenced as the column names or number of x@data
#' @param vars the axes of the embedding to use for plotting
#' @param ... handed over to the underlying plotting function.
#'
#' @examples
#' scurve = loadDataSet("3D S Curve")
#' plot(scurve, type = "pairs", main = "pairs plot of S curve")
#' plot(scurve, type = "parpl")
#' plot(scurve, type = "2vars", vars = c("y", "z"))
#' plot(scurve, type = "3vars")
#'
#' @include mixColorSpaces.R
#' @include dimRedData-class.R
#' @importFrom graphics plot
#' 
#'
#' @export
setGeneric(
    'plot', function(x, y, ...) standardGeneric('plot'),
    useAsDefault = graphics::plot
)

#' @describeIn plot Plot dimRedData objects
#' 
#' @export
setMethod(
    f = 'plot',
    signature = c('dimRedData'),
    definition = function(x, type = "pairs",
                          vars = seq_len(ncol(x@data)),
                          col = seq_len(min(3, ncol(x@meta))), ...) {
        cols <- colorize(x@meta[, col, drop = FALSE])
        switch(
            type,
            "pairs"    = {
                chckpkg("graphics")
                graphics::pairs(x@data[,vars],      col = cols,   ... )
            },
            "parpl"    = {
                chckpkg("MASS")
                MASS::parcoord(x@data[,vars],      col = cols,   ... )
            },
            "2vars"    = {
                chckpkg("graphics")
                graphics::plot(x@data[,vars[1:2]], col = cols,   ... )
            },
            "3vars"    = {
                if(!requireNamespace("scatterplot3d")) stop("require scatterplot3d package")
                scatterplot3d::scatterplot3d(x@data[,vars[1:3]], color = cols, ... )
            },
            "3varsrgl" = {
                chckpkg("rgl")
                rgl::points3d(x@data[,vars[1:3]], col = cols,   ... )
            },
            stop("wrong argument to plot.dimRedData")
        )
    }
)


#' @describeIn plot plot dimRedResult objects
#' @export
setMethod(
    f = 'plot',
    signature = c('dimRedResult'),
    definition = function (x, type = "pairs",
                           vars = seq_len(ncol(x@data@data)),
                           col = seq_len(min(3, ncol(x@data@meta))), ...) {
        plot(x = x@data, type = type, vars = vars, col = col, ...)
    }
)
