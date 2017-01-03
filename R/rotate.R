
## rotate X in such a way that the values of Y have maximum squared
## correlation with the dimensions specified in axes. We optimize
## axes[1] first, then axes[2] without axes[1], ...

## we maximize the squared correlations of the original variables
## with the axis of the embeding and the final result is the sum_{axes} sum(squared(correlation(variables, axis)))

setGeneric(
    "maximize_correlation",
    function(object, ...) standardGeneric("maximize_correlation"),
    valueClass = "dimRedResult"
)

#' Maximize Correlation with the Axes
#'
#' Rotates the data in such a way that the correlation with the first
#' \code{naxes} axes is maximized.
#'
#' Methods that do not use eigenvector decomposition, like t-SNE often
#' do not align the data with axes according to the correlation of
#' variables with the data. \code{maximize_correlation} uses the
#' \code{\link[optimx]{optimx}} package to rotate the data in such a
#' way that the original variables have maximum correlation with the
#' embedding axes.
#'
#' @param object A dimRedResult object
#' @param naxes the number of axes to optimize for.
#' @param cor_method which correlation method to use
#'
#' @aliases maximize_correlation
#' @export
setMethod(
    "maximize_correlation",
    "dimRedResult",
    function(object, naxes = ncol(object@data@data), cor_method = "pearson"){
        ## if (missing(naxes))      naxes      <- ncol(object@data@data)
        ## if (missing(cor_method)) cor_method <- "pearson"

        if (!object@has.org.data) stop("object requires original data")
        if (length(naxes) != 1 || naxes < 1 || naxes > ncol(object@data@data))
            stop("naxes must specify the numbers of axes to optimize for, ",
                 "i.e. a single integer between 1 and ncol(object@data@data)")
        ## try to partially match cor_method:
        cor_method <-
            cor_method[pmatch(cor_method, c("pearson", "kendall", "spearman"))]
        if (is.na(cor_method))
            stop("cor_method must match one of ",
                 "'pearson', 'kendall', or 'spearman', ",
                 "at least partially.")

        mcres <- .maximize_correlation(object@data@data,
                                       object@org.data,
                                       1:naxes,
                                       cor_method)

        res <- object
        res@data@data <- mcres$rotated
        return(res)
    }
)

.maximize_correlation <- function(X, Y,
                                  axes = 1:ncol(X),
                                  cor_method = "pearson"){

  if (nrow(X) != nrow(Y))
    stop("'X' and 'Y' must have the same number of rows")
  if (max(axes) > ncol(X)){
    axes <- axes[ axes <= ncol(X) ]
    warning("'max(axes)' must be <= 'ncol(X)', removing some axes")
  }

  chckpkg("optimx")

  xndim <- ncol(X)
  without_axes <- integer(0)
  res <- list()

  for (axis in axes){
    without_axes <- c(without_axes, axis)

    nplanes <- xndim - length(without_axes)
    planes <- matrix(NA, 2, nplanes)
    planes[1, ] <- axis
    planes[2, ] <- (1:xndim)[-without_axes]
    if (ncol(planes) == 0)
      break

    o <- optimx::optimx(
      par = rep(0, nplanes),
      fn = obj,
      ## method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "nlm",
      ##            "nlminb", "spg", "ucminf", "newuoa", "bobyqa", "nmkb",
      ##            "hjkb", "Rcgmin", "Rvmmin"),
      lower = 0,
      upper = 2 * pi,
      control = list(all.methods = T),
      X = as.matrix(X),
      Y = as.matrix(Y),
      axis = axis,
      without_axes = without_axes,
      cor_method = cor_method
    )

    best_idx <- which.min(o$value)

    if (length(best_idx) == 0)
      best_idx <- NA

    res_idx <- length(res) + 1
    res[[res_idx]] <- list()
    res[[res_idx]]$axis <- axis
    res[[res_idx]]$without_axes <- without_axes
    res[[res_idx]]$angs <- unname( unlist(o[best_idx, 1:nplanes]) )
    res[[res_idx]]$planes <- planes
    res[[res_idx]]$X <- rotate(res[[res_idx]]$angs, planes, X)
    ## this is the mean squared correlation of the original variables
    ## with "axis", see return value of "obj":
    res[[res_idx]]$cor <- -o$value[best_idx]
  }

  ## calculate the correlation for axes
  nres <- length(res)
  if (nres > 0) {
    ## the result is the sum of the mean squared correlations of the
    ## original variables with the axes. "res[[i]]$cor" contains the
    ## mean squared correlation of the variables with axis "i"
    res$result <- 0
    for (i in 1:nres)
      res$result <- res$result + res[[i]]$cor ^ 2
    ## res$result <- res$result / length(res)

    ## rotate the input to maximize correlations
    res$rotated <- X
    for (i in 1:nres)
      res$rotated <- rotate(res[[i]]$angs, res[[i]]$planes, res$rotated)
  } else {
    ## if we only had one dimension, simply return the means squared
    ## correlation and don't rotate
    res$result <- sum(correlate(X, Y, cor_method) ^ 2)
    res$rotated <- X
  }

  res
}




#### helper functions for rotation

## we create a number or rotation matrices around the 2d planes
## spanned by the orthonormal matrices, multiply them for a general
## rotation which is then applied to the data X
rotate <- function (angs, planes, X) {
  ndim <- ncol(X)
  nplanes <- ncol(planes)
  if (length(angs) != nplanes)
    stop("length(angs) not equal to chose(ndim, 2)")

  ## loop over the planes to construct general rotation matrix
  rotmat <- diag(ndim)
  for (p in 1:nplanes) {
    ## 2d rotation
    ## possible optimization: create large rotation matrix
    ## directly and insert values linearly without a for loop
    rotmat2d <- matrix(
      c(cos(angs[p]), -sin(angs[p]),
        sin(angs[p]),  cos(angs[p])),
      2, 2, byrow = T
    )
    p_rotmat <- diag(ndim)
    for (i in 1:2)
      for (j in 1:2)
        p_rotmat[ planes[i, p], planes[j, p] ] <- rotmat2d[i, j]
    rotmat <- rotmat %*% p_rotmat
  }

  t(rotmat %*% t(X))
}

get_planes <- function(ndims, axis, without_axes){
  nplanes <- ndims - length(without_axes)
  planes <- matrix(NA, 2, nplanes)
  planes[1, ] <- axis
  planes[2, ] <- (1:ndims)[c(-axis, -without_axes)]
  planes
}


obj <- function(alpha, X, Y, axis, without_axes, cor_method = "pearson"){
  ## correlation with first axis
  xndim <- ncol(X)

  planes <- get_planes(xndim, axis, without_axes)

  X2 <- rotate(alpha, planes, X)


  ## cor(x, y) returns a matrix with the correlations between the
  ## columns of x = X2 (rows) and the columns of y = Y (columns) we
  ## want the mean of squared correlations of all variables original
  ## variables with the first axis, i.e. we require the relevant
  ## (axis) column of the resulting matrix.

  ## Possible optimization: use only the relevant column of Y

  -mean(correlate(
    X2, Y,
    #use = "pairwise.complete.obs",
    method = cor_method
  )[axis, ] ^ 2)
}

correlate <- function (x, y, method, ...) {
  if (method != "kendall"){
      return(stats::cor(x, y, method = method, ...))
  } else {
      chckpkg("pcaPP")
      ## make the cor.fk method behave like cor for matrices:
      if (is.matrix(x) && is.matrix(y)) {
          res <- matrix(
              NA, nrow = ncol(x), ncol = ncol(y),
              dimnames = list(colnames(x), colnames(y))
          )
          for (i in 1:ncol(x)) {
              for (j in 1:ncol(y)){
                  res[i, j] <- pcaPP::cor.fk(x[, i], y[, j])
              }
          }
          return(res)
      } else if (is.null(dim(x)) && is.null(dim(y))){
          return(pcaPP::cor.fk(x, y))
      } else {
          stop("something is wrong with the input of 'correlate()'")
      }
  }
}
