#' Fortify network objects.
#'
#' A wrapper for the \code{\link{fortify.network}} and
#' \code{\link{fortify.igraph}} functions that will also try to coerce matrices
#' and data frames to network objects.
#'
#' @param x an object of class \code{\link[network]{network}} or
#'   \code{\link[igraph:igraph-package]{igraph}}, or any object that can be
#'   coerced to that class, such as an adjacency or incidence matrix, or an
#'   edge list: see \code{\link[network]{edgeset.constructors}} and
#'   \code{\link[network]{network}} for details.
#' @param ... arguments passed to the \code{\link{fortify.network}} or
#'   \code{\link{fortify.igraph}} functions.
#'
#' @export
ggnetwork <- function(x, ...) {
  switch(
    EXPR = data.class(x),
    "igraph" = fortify.igraph(x, ...),
    "network" = fortify.network(x, ...),
    tryCatch(
      fortify.network(network::network(x), ...),
      error = function(e) {
        stop('could not coerce object to a network: see ?ggnetwork for help')
      }
    )
  )
}
