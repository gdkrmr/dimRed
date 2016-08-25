#' Plotting of dimRed* objects
#'
#' Plots a object of class dimRedResult and dimRedData
#'
#' somewhat inflexible, if you require custom colors or want to modify
#' graphical parameters, write your own plotting function.
#'
#' @param x dimRedResult/dimRedData class, e.g. output of
#'     embedded/loadDataSet
#' @param type plot type, one of \code{c("pairs", "parallel", "2d",
#'     "3d")}
#' @param colors the columns of the meta slot to use for coloring
#' @param vars the axes of the embedding to use for plotting
#'
#' @examples
#' scurve = loadDataSet("3D S Curve")
#' plot(scurve, "pairs")
#'
#' @include mixColorSpaces.R
#' @include dimRedData-class.R
#' 
#' @export
setMethod(
    f = 'plot',
    signature = 'dimRedData',
    definition = function(x, y, ...) {
        requireNamespace("MASS")
        requireNamespace("rgl")
        requireNamespace("graphics")
        if(!hasArg(type))   type <- "pairs"
        if(!hasArg(colors)) colors <- seq_len(min(3, ncol(x@meta)))
        if(!hasArg(vars))   vars <- seq_len(ncol(x@data))
        cols <- colorize(x@meta[,colors])
        switch(
            type,
            "pairs" = graphics::pairs( x@data[,vars], gap = 0, col = cols ),
            "parpl" = MASS::parcoord(  x@data[,vars],          col = cols ),
            "2vars" = graphics::plot(  x@data[,vars[1:2]],     col = cols ),
            "3vars" = rgl::points3d(   x@data[,vars[1:3]],     col = cols ),
            stop("wrong argument to plot.dimRedData")
        )
    }
)

