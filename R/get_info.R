


#' getRotationMatrix
#'
#' Extract the rotation matrix from \code{\link{dimRedResult}} objects derived from PCA and FastICA
#'
#' The data has to be pre-processed the same way as the method does, e.g.
#' centering and/or scaling.
#'
#' @param x of type \code{\link{dimRedResult}}
#' @return a matrix
#'
#' @examples
#' dat <- loadDataSet("Iris")
#'
#' pca <- embed(dat, "PCA")
#' ica <- embed(dat, "FastICA")
#'
#' rot_pca <- getRotationMatrix(pca)
#' rot_ica <- getRotationMatrix(ica)
#'
#' scale(getData(dat), TRUE, FALSE) %*% rot_pca - getData(getDimRedData(pca))
#' scale(getData(dat), TRUE, FALSE) %*% rot_ica - getData(getDimRedData(ica))
#'
#' @family convenience functions
#' @export
getRotationMatrix <- function(x) {
  if(!inherits(x, "dimRedResult")) stop("x must be of type 'dimRedResult'")
  if(x@method == "PCA")     return(environment(x@apply)$rot)
  if(x@method == "PCA_L1")  return(environment(x@apply)$rot)
  if(x@method == "FastICA") return(environment(x@apply)$res$K %*% environment(x@apply)$res$W)
  stop(paste("Not implemented for", x@method))
}
