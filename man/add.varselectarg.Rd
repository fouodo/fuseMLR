% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscellaneous.varselectarg.R
\name{add.varselectarg}
\alias{add.varselectarg}
\title{This function will add a new list of argument to the current object given as parameter.}
\usage{
\method{add}{varselectarg}(object, layer, param, ...)
}
\arguments{
\item{object}{The current lrnarg object.}

\item{layer}{New layer name to be added.}

\item{param}{New parameter list.}

\item{...}{More arguments to be passed to further function.}
}
\value{
Object from class \code{varselectarg}.
}
\description{
This function will add a new list of argument to the current object given as parameter.
}
\examples{
test_varselectarg <- varselectarg(object = list(probability = TRUE),
                         layer = "methylation")
test_varselectarg <- add(object = test_varselectarg,
                  layer = "genexpr",
                  param = list(probability = TRUE))
}
