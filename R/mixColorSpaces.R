



#' Mixing color ramps
#'
#' mix different color ramps
#'
#' automatically create colors to represent a varying number of
#' dimensions.
#' 
#'
#'
#' @examples 
#' cols <- expand.grid(x = seq(0,1, length.out = 10),
#'                     y = seq(0,1, length.out = 10),
#'                     z = seq(0,1, length.out = 10))
#' mixed <- mixColor3Ramps(cols)
#'
#' \dontrun{
#' library(rgl)
#' plot3d(cols$x, cols$y, cols$z, col = mixed, pch = 15)
#'
#' cols <- expand.grid(x = seq(0,1, length.out = 10),
#'                     y = seq(0,1, length.out = 10))
#' mixed <- mixColor2Ramps(cols)
#' }
#' 
#' plot(cols$x, cols$y, col = mixed, pch = 15)
#' @importFrom grDevices colorRamp
#' @importFrom grDevices rgb
#' @export
mixColorRamps <- function (vars, ramps) {
    if(length(vars) > length(ramps)) stop("need more or equal ramps than vars")
   
    nvars <- length(vars)
    
    rgbs <- list()
    for (i in 1:nvars){
        rgbs[[i]] <- ramps[[i]](scale01(as.numeric(vars[[i]])))
    }

    retrgb <- Reduce(`+`, rgbs)

    res <- apply(retrgb, 2,  function(x) (x - min(x)) / (max(x) - min(x)))
    res[is.nan(res)] <- 0
   
    return(rgb(res))    
}

#' @rdname mixColorRamps
#' @export
mixColor1Ramps <- function (var,
                            ramp = colorRamp(c('blue', 'black', 'red'))) {
    mixColorRamps(var, list(ramp))
}

#' @rdname mixColorRamps
#' @export
mixColor2Ramps <- function (vars,
                            ramps = list(colorRamp(c('blue', 'green')),
                                         colorRamp(c('blue', 'red')))) {
    mixColorRamps(vars, ramps)
}

#' @rdname mixColorRamps
#' @export
mixColor3Ramps <- function (vars,
                           ramps = list(colorRamp(c('white','green')),
                                        colorRamp(c('white','blue')),
                                        colorRamp(c('white','red')))) {
    mixColorRamps(vars, ramps)
}

colorize <- function (vars) {
    l <- length(vars)
    if(l == 1) return(mixColor1Ramps(vars))
    if(l == 2) return(mixColor2Ramps(vars))
    if(l == 3) return(mixColor3Ramps(vars))
    return('#000000')
}

scale01 <- function(x, low = min(x, na.rm = TRUE), high = max(x, na.rm = FALSE)) {
    x <- (x - low)/(high - low)
    x
}
