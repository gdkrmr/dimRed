% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_info.R
\name{getRotationMatrix}
\alias{getRotationMatrix}
\title{getRotationMatrix}
\usage{
getRotationMatrix(x)
}
\arguments{
\item{x}{of type \code{\link{dimRedResult}}}
}
\value{
a matrix
}
\description{
Extract the rotation matrix from \code{\link{dimRedResult}} objects derived from PCA and FastICA
}
\details{
The data has to be pre-processed the same way as the method does, e.g.
centering and/or scaling.
}
\examples{
dat <- loadDataSet("Iris")

pca <- embed(dat, "PCA")
rot_pca <- getRotationMatrix(pca)
scale(getData(dat), TRUE, FALSE) \%*\% rot_pca - getData(getDimRedData(pca))


if(requireNamespace("fastICA", quietly = TRUE)) {
  ica <- embed(dat, "FastICA")
  rot_ica <- getRotationMatrix(ica)
  scale(getData(dat), TRUE, FALSE) \%*\% rot_ica - getData(getDimRedData(ica))
}


}
\concept{convenience functions}
