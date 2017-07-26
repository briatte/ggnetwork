#' Fortify method for networks of class \code{\link[igraph:igraph-package]{igraph}}
#'
#' This function requires the \code{\link[intergraph:intergraph-package]{intergraph}}
#' package to be installed.
#' @param model an object of class \code{\link[igraph:igraph-package]{igraph}}
#' @param layout a function call to an 
#' \code{\link[igraph:igraph-package]{igraph}} layout function.  See 
#' \code{\link[igraph]{layout_}} for details.
#' @param arrow.gap a parameter that will shorten the network edges in order to
#' avoid overplotting edge arrows and nodes; defaults to \code{0} when the
#' network is undirected (no edge shortening), or to \code{0.025} when the
#' network is directed. Small values near \code{0.025} will generally achieve
#' good results when the size of the nodes is reasonably small.
#' @param by a character vector that matches an edge attribute, which will be
#' used to generate a data frame that can be plotted with
#' @param ... additional parameters for the \code{layout} argument; see
#' \code{\link[sna]{gplot.layout}} for available options.
#' @method fortify igraph
#' @importFrom utils installed.packages
#' @export
fortify.igraph <- function(model, layout = igraph::nicely(), 
                           arrow.gap = ifelse(network::is.directed(x), 0.025, 0),
                           by = NULL) {

  if ("intergraph" %in% rownames(utils::installed.packages())) {

    fortify.network(intergraph::asNetwork(model), ...)

  } else {

    stop("install the 'intergraph' package to use igraph objects with ggnetwork")

  }

}
