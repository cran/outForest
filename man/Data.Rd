% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{Data}
\alias{Data}
\alias{Data.default}
\alias{Data.outForest}
\title{Extracts Data}
\usage{
Data(object, ...)

\method{Data}{default}(object, ...)

\method{Data}{outForest}(object, ...)
}
\arguments{
\item{object}{An object of class "outForest".}

\item{...}{Arguments passed from or to other methods.}
}
\value{
A \code{data.frame}.
}
\description{
Extracts data with optionally replaced outliers from object of class "outForest".
}
\section{Methods (by class)}{
\itemize{
\item \code{Data(default)}: Default method not implemented yet.

\item \code{Data(outForest)}: Extract data from "outForest" object.

}}
\examples{
x <- outForest(iris)
head(Data(x))
}
