% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{installSuggests}
\alias{installSuggests}
\title{getSuggests}
\usage{
installSuggests(...)
}
\arguments{
\item{...}{additional options passed to install.packages.}
}
\description{
Install packages wich are suggested by dimRed.
}
\details{
By default dimRed will not install all the dependencies, because
there are quite a lot and in case some of them are not available
for your platform you will not be able to install dimRed without
problems.

To solve this I provide a function which automatically installes
all the suggested packages.
}
\examples{
\dontrun{
installSuggests()
}
}
