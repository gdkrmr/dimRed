#' Plotting of dimRed* objects
#'
#' Plots a object of class dimRedResult and dimRedData. For the
#' documentation of the plotting function in base see here:
#' \code{\link{plot.default}}.
#'
#' Plotting functions for the classes usind in \code{dimRed}. they are
#' intended to give a quick overview over the results, so they are
#' somewhat inflexible, e.g. it is hard to modify color scales or
#' plotting parameters.
#'
#' If you require more control over plotting, it is better to convert
#' the object to a \code{data.frame} first and use the standard
#' functions for plotting.
#'
#' @param x dimRedResult/dimRedData class, e.g. output of
#'     embedded/loadDataSet
#' @param y Ignored
#' @param type plot type, one of \code{c("pairs", "parallel", "2vars",
#'     "3vars", "3varsrgl")}
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
#' @aliases plot.dimRed
#' @export
setGeneric(
    "plot", function(x, y, ...) standardGeneric("plot"),
    useAsDefault = graphics::plot
)

#' @describeIn plot Ploting of dimRedData objects
#' @aliases plot.dimRedData
#' @export
setMethod(
    f = "plot",
    signature = c("dimRedData"),
    definition = function(x, type = "pairs",
                          vars = seq_len(ncol(x@data)),
                          col = seq_len(min(3, ncol(x@meta))), ...) {
        cols <- colorize(x@meta[, col, drop = FALSE])
        switch(
            type,
            "pairs"    = {
                chckpkg("graphics")
                graphics::pairs(x@data[, vars],      col = cols,   ... )
            },
            "parpl"    = {
                chckpkg("MASS")
                MASS::parcoord(x@data[, vars],      col = cols,   ... )
            },
            "2vars"    = {
                chckpkg("graphics")
                graphics::plot(x@data[, vars[1:2]], col = cols,   ... )
            },
            "3vars"    = {
                chckpkg("scatterplot3d")
                scatterplot3d::scatterplot3d(x@data[, vars[1:3]],
                                             color = cols,
                                             ...)
            },
            "3varsrgl" = {
                chckpkg("rgl")
                rgl::plot3d(x@data[, vars[1:3]], col = cols,   ... )
            },
            stop("wrong argument to plot.dimRedData")
        )
    }
)


#' @describeIn plot Ploting of dimRedResult objects.
#' @aliases plot.dimRedResult
#' @export
setMethod(
    f = "plot",
    signature = c("dimRedResult"),
    definition = function (x, type = "pairs",
                           vars = seq_len(ncol(x@data@data)),
                           col = seq_len(min(3, ncol(x@data@meta))), ...) {
        plot(x = x@data, type = type, vars = vars, col = col, ...)
    }
)

#' plot RN_X
#'
#' 
#' 
#' 
#' @export
plot_RN_X <- function(x) {
    chckpkg("ggplot2")
    chckpkg("tidyr")
    chckpkg("scales")
    lapply(
        x,
        function(x)
        if (!inherits(x, "dimRedResult"))
            stop("x must be a list and ",
                 "all items must inherit from 'dimRedResult'")
    )

    rnx <- lapply(x, R_NX)
    auc <- sapply(rnx, auc_lnK)

    df <- as.data.frame(rnx)
    names(df) <- paste(format(auc, digits = 4),
                       names(x))
    df$K <- seq_len(nrow(df))

    qnxgrid <- expand.grid(K = df$K,
                           rnx = seq(0.1, 0.9, by = 0.1))
    ## TODO: FIND OUT WHY THIS AS IN THE PUBLICATION BUT IS WRONG!
    qnxgrid$qnx <- qnx2rnx(qnxgrid$rnx, K = qnxgrid$K, N = nrow(df))
    qnxgrid$rnx_group <- factor(qnxgrid$rnx)

    df <- tidyr::gather(df, key = "embedding", value = "R_NX", -K)

    ggplot2::ggplot(df) +
        ggplot2::geom_line(ggplot2::aes(y = R_NX, x = K,
                                        color = embedding)) +
        ggplot2::geom_line(data = qnxgrid,
                           mapping = ggplot2::aes(x = K, y = qnx,
                                                  group = rnx_group),
                           linetype = 2,
                           size = 0.1) +
        ggplot2::geom_line(data = qnxgrid,
                           mapping = ggplot2::aes(x = K, y = rnx,
                                                  group = rnx_group),
                           linetype = 2,
                           size = 0.1) +
        ggplot2::scale_x_log10(
            labels = scales::trans_format("log10",
                                          scales::math_format(10 ^ .x)),
            expand = c(0, 0)
        ) +
        ggplot2::scale_y_continuous(limits = c(0, 1),
                           expand = c(0, 0)) +
        ggplot2::annotation_logticks(sides = "b") +
        ggplot2::theme_classic()
}
