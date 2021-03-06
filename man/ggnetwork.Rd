% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggnetwork.R
\name{ggnetwork}
\alias{ggnetwork}
\title{Fortify network objects.}
\usage{
ggnetwork(x, ...)
}
\arguments{
\item{x}{an object of class \code{\link[network]{network}} or
\code{\link[igraph:igraph-package]{igraph}}, or any object that can be
coerced to that class, such as an adjacency or incidence matrix, or an
edge list: see \code{\link[network]{edgeset.constructors}} and
\code{\link[network]{network}} for details.}

\item{...}{arguments passed to the \code{\link{fortify.network}} or
\code{\link{fortify.igraph}} functions.}
}
\description{
A wrapper for the \code{\link{fortify.network}} and
\code{\link{fortify.igraph}} functions that will also try to coerce matrices
and data frames to network objects.
}
