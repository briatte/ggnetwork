#' Fortify network objects.
#'
#' A wrapper for the \code{\link{fortify.network}} and
#' \code{\link{fortify.igraph}} functions that will also try to coerce matrices
#' and data frames to network objects.
#' @param x an object of class \code{\link[network]{network}}, or any object
#' that can be coerced to this class, such as an adjacency or incidence matrix,
#' or an edge list: see \code{\link[network]{edgeset.constructors}} and
#' \code{\link[network]{network}} for details. If the object is of class
#' \code{\link[igraph:igraph-package]{igraph}} and the
#' \code{\link[intergraph:intergraph-package]{intergraph}} package is installed,
#' it will be used to convert the object: see
#' \code{\link{fortify.igraph}} for details.
#' @param ... arguments passed to the \code{\link{fortify.network}} function.
#' @rdname ggnetwork
#' @export
ggnetwork <- function(x, ...) {
    UseMethod('ggnetwork')
}

#' @export
#' @rdname ggnetwork
#' @method ggnetwork igraph
ggnetwork.igraph <- function(x, ...) {

    fortify.igraph(x, ...)

} 

#' @export
#' @rdname ggnetwork
#' @method ggnetwork default
ggnetwork.default <- function(x, ...) {

    if (!network::is.network(x)) {
        x <- try(network::network(x), silent = TRUE)
    }

    if (!network::is.network(x)) stop("could not coerce object to a network")

    fortify.network(x, ...)

}


