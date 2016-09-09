#' Example Data Sets for dimensionality reduction
#'
#' A compilation of standard data sets that are often being used to
#' showcase dimensionality reduction techniques.
#'
#' Name should be one of \code{dataSetList()}. Partial matching is
#' possible, see \code{\link{match.arg}}. Generated data sets contain
#' the internal coordinates of the manifold in the \code{meta} slot.
#' 
#'
#' 
#' @param name A character vector that specifies the name of the data
#'     set.
#' @param n In generated data sets the number of points to be
#'     generated.
#' @param sigma Standard deviation of the noise added.
#' @return \code{loadDataSet} an object of class
#'     \code{\link{dimRedData}}. \code{dataSetList()} return a
#'     character string with the implemented data sets
#'
#' @examples
#' ## a list of available data sets:
#' dataSetList()
#' ## Load a data set:
#' swissRoll <- loadDataSet("Swiss Roll")
#' \donttest{plot(swissRoll, type = '3vars')}
#' ## Load Iris data set, partial matching:
#' loadDataSet("I")
#' 
#' @import stats
#' @import methods
#' 
#' @name dataSets
NULL

#' @include dimRedData-class.R
#'
#' @rdname dataSets
#' 
#' @export
loadDataSet <- function (name = dataSetList(), n = 2000, sigma = 0.05) {
    name <- match.arg(name)
    switch(
        name,
        'Swiss Roll'           = swissRoll(n, sigma),
        'Broken Swiss Roll'    = brokenSwissRoll(n, sigma),
        'Helix'                = helix(n, sigma),
        'Twin Peaks'           = twinPeaks(n, sigma),
        'Sphere'               = sphere(n, sigma),
        'Ball'                 = ball(n, sigma),
        '3D S Curve'           = sCurve(n, sigma),
        'variable Noise Helix' = noisyHelix(n, sigma),
        'Iris'                 = irisdata()
    )
}

#' @rdname dataSets
#' 
#' @export
dataSetList <- function () {
    return(c(
        'Swiss Roll',
        'Broken Swiss Roll',
        'Helix',
        'Twin Peaks',
        'Sphere',
        'Ball',
        '3D S Curve',
        'variable Noise Helix',
        'Iris'
    ))
}

irisdata <- function() {
    ##utils::data(iris)
    dd <- as.matrix(datasets::iris[, 1:4])
    new('dimRedData',
        data = dd,
        meta = datasets::iris[, 5, drop = FALSE])
}

swissRoll <- function (n = 2000, sigma = 0.05) {
    x <- runif(n, 1.5 * pi, 4.5 * pi)
    y <- runif(n, 0, 30)
    
    new('dimRedData',
        data = swissRollMapping(x, y) + rnorm(3*n, sd = sigma),
        meta = data.frame(x = x, y = y))
}

brokenSwissRoll <- function (n = 2000, sigma = 0.05) {
    x <- c(
        runif(floor(n/2),   1.5 * pi, 2.7 * pi),
        runif(ceiling(n/2), 3.3 * pi, 4.5 * pi)
    )
    y <- runif(n, 0, 30)
    
    new('dimRedData',
        data = swissRollMapping(x, y) + rnorm(3*n, sd = sigma),
        meta = data.frame(x = x, y = y))
}

swissRollMapping <- function (x, y) {
    cbind(x = x * cos(x),
          y = y,
          z = x * sin(x))
}


helix <- function (n = 2000, sigma = 0.05) {
    t <- runif(n, 0, 2 * pi)
    new('dimRedData',
        data = helixMapping(t) + rnorm(3*n, sd = sigma),
        meta = data.frame(t = t))
}

helixMapping <- function (x) {
    cbind(x = (2 + cos(8*x)) * cos(x),
          y = (2 + cos(8*x)) * sin(x),
          z = (sin(8*x)))
}
    
twinPeaks <- function (n = 2000, sigma = 0.05) {
    x <- runif(n, -1, 1)
    y <- runif(n, -1, 1)

    new('dimRedData',
        data = twinPeaksMapping(x, y) + rnorm(3*n, sd = sigma),
        meta = data.frame(x = x, y = y))
}

twinPeaksMapping <- function (x, y) {
    cbind(x = x,
          y = y,
          z = sin(pi*x) * tanh(3*y))
}


sphere <- function (n = 2000, sigma = 0.05) {
    phi <- runif(n, 0, 2*pi)
    psi <- acos(runif(n, -1, 1))
    
    new('dimRedData',
        data = sphereMapping(phi, psi) + rnorm(3*n, sd = sigma),
        meta = data.frame(phi = phi, psi = psi))
}

sphereMapping <- function (phi, psi) {
    cbind(x = cos(phi)*sin(psi),
          y = sin(phi)*sin(psi),
          z = cos(psi))
}

ball <- function (n = 2000, sigma = 0.05) {
    ## ## the resampling method, not using it, because it destroys the
    ## original dimensional information.
    ## x <- numeric(n)
    ## y <- numeric(n) z <- numeric(n) outside <- rep(TRUE, n)
    ## repeat {
    ##     no <- sum(outside)
    ##     if (no == 0) break
    ##     x[outside] <- runif(no, -1, 1)
    ##     y[outside] <- runif(no, -1, 1)
    ##     z[outside] <- runif(no, -1, 1)
    ##     outside <- sqrt(x^2 + y^2 + z^2) > 1
    ## }
    ## new('dimRedData',
    ##     data = cbind(x = x, y = y, z = z) + rnorm(3*n, sd = sigma)
    ##     meta = )

    ## the following method is the analytical one:
    phi <- runif(n, 0, 2*pi)
    psi <- acos(runif(n, -1, 1))
    ## make it uniformly distributed inside the sphere
    r <-  runif(n)^(1/3)
    
    new('dimRedData',
        data = ballMapping(phi, psi, r) + rnorm(3*n, sd = sigma),
        meta = data.frame(phi = phi, psi = psi, r = r))
}

ballMapping <- function (phi, psi, r) {
    cbind(x = r*cos(phi)*sin(psi),
          y = r*sin(phi)*sin(psi),
          z = r*cos(psi))    
}

sCurve <- function (n = 2000, sigma = 0.05) {
    t <- runif(n, -1.5*pi, 1.5*pi)
    y <- runif(n, 0, 2)

    new('dimRedData',
        data = sCurveMapping(t, y) + rnorm(3*n, sd = sigma),
        meta = data.frame(x = t, y = y))
}

sCurveMapping <- function (t, y) {
    cbind(x = sin(t),
          y = y,
          z = sign(t) * (cos(t) - 1))
}

noisyHelix <- function (n = 2000, sigma = 0.05) {
    t <- runif(n, 0, 4*pi)
    min_noise <- 0.1
    max_noise <- 1.8
  
    new('dimRedData',
        data = noisyHelixMapping(t, min_noise, max_noise) + rnorm(3*n, sd = sigma),
        meta = data.frame(t = t))
}

noisyHelixMapping <- function(t, min_noise, max_noise) {
    make_noise <- function (t){
        rnorm(length(t), sd = t * max_noise / max(t) + min_noise)
    }
    
    cbind(x = 3 * cos(t) + make_noise(t),
          y = 3 * sin(t) + make_noise(t),
          z = 2 * t + make_noise(t))
}

