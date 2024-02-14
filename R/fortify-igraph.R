#' Fortify method for networks of class \code{\link[igraph:igraph-package]{igraph}}
#'
#' @param model an object of class \code{\link[igraph:igraph-package]{igraph}}.
#' @param data not used by this method.
#' @param layout a function call to an
#'   \code{\link[igraph:igraph-package]{igraph}} layout function, such as
#'   \code{\link[igraph]{layout_nicely}} (the default), or a 2 column matrix
#'   giving the x and y coordinates for the vertices.
#'   See \code{\link[igraph]{layout_}} for details.
#' @inheritParams format_fortify
#' @param ... additional parameters for the \code{\link[igraph]{layout_}} function
#'
#' @return a \code{\link[base]{data.frame}} object.
#'
#' @export
fortify.igraph <- function(
  model,
  data = NULL,
  layout = igraph::nicely(),
  arrow.gap = ifelse(igraph::is.directed(model), 0.025, 0),
  by = NULL,
  scale = TRUE,
  stringsAsFactors = getOption("stringsAsFactors", FALSE),
  ...
) {
  # node placement
  if (inherits(layout, "matrix") && identical(dim(layout), c(as.integer(igraph::gorder(model)), 2L))) {
    nodes <- layout[, 1:2 ]
  } else if (inherits(layout, "matrix")) {
    stop("layout matrix dimensions do not match network size")
  } else {
    nodes <- igraph::layout_(model, layout, ...)
  }

  format_fortify(
    model = model,
    nodes = nodes,
    weights = "none",
    arrow.gap = arrow.gap,
    by = by,
    scale = scale,
    stringsAsFactors = stringsAsFactors,
    .list_vertex_attributes_fun = igraph::list.vertex.attributes,
    .get_vertex_attributes_fun = igraph::get.vertex.attribute,
    .list_edges_attributes_fun = igraph::list.edge.attributes,
    .get_edges_attributes_fun = igraph::get.edge.attribute,
    .as_edges_list_fun = igraph::as_edgelist
  )
}
