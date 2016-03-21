#' Convert an igraph object to a data frame.
#'
#' This function requires the \code{\link[intergraph:intergraph-package]{intergraph}}
#' package to be installed.
#' @param model an object of class \code{\link[igraph:igraph-package]{igraph}},
#' which will be converted to an object of class \code{\link[network]{network}}
#' with the \code{\link[intergraph]{asNetwork}} function before being passed to
#' the \code{\link{fortify.network}} function.
#' @param ... additional parameters for the \code{fortify.network} function; see
#' \code{\link{fortify.network}}.
#' @method fortify igraph
#' @export
fortify.igraph <- function(model, ...) {

  if ("intergraph" %in% rownames(installed.packages())) {

    fortify.network(intergraph::asNetwork(model), ...)

  } else {

    stop("install the 'intergraph' package to use igraph objects with ggnetwork")

  }

}
