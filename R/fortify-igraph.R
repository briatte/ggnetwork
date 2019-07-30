#' Fortify method for networks of class \code{\link[igraph:igraph-package]{igraph}}
#'
#' @param model an object of class \code{\link[igraph:igraph-package]{igraph}}
#' @param data not used by this method.
#' @param layout a function call to an
#'   \code{\link[igraph:igraph-package]{igraph}} layout function, such as
#'   \code{\link[igraph]{layout_nicely}} (the default), or a 2 column matrix
#'   giving the x and y coordinates for the vertices.
#'   See \code{\link[igraph]{layout_}} for details.
#' @param arrow.gap a parameter that will shorten the network edges in order to
#'   avoid overplotting edge arrows and nodes; defaults to \code{0} when the
#'   network is undirected (no edge shortening), or to \code{0.025} when the
#'   network is directed. Small values near \code{0.025} will generally achieve
#'   good results when the size of the nodes is reasonably small.
#' @param by a character vector that matches an edge attribute, which will be
#'   used to generate a data frame that can be plotted with
#'   \code{\link[ggplot2]{facet_wrap}} or \code{\link[ggplot2]{facet_grid}}. The
#'   nodes of the network will appear in all facets, at the same coordinates.
#'   Defaults to \code{NULL} (no faceting).
#' @param stringsAsFactors whether vertex and edge attributes should be
#'   converted to factors if they are of class \code{character}. Defaults to
#'   the value of \code{getOption("stringsAsFactors")}, which is \code{TRUE} by
#'   default: see \code{\link[base]{data.frame}}.
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
  stringsAsFactors = getOption("stringsAsFactors"),
  ...
) {
  # node placement
  if (class(layout) == "matrix" && identical(dim(layout), c(igraph::gorder(model), 2L))) {
    nodes <- layout[, 1:2 ]
  } else if (class(layout) == "matrix") {
    stop("layout matrix dimensions do not match network size")
  } else {
    nodes <- igraph::layout_(model, layout, ...)
  }

  # store coordinates
  nodes <- data.frame(nodes)
  names(nodes) <- c("x", "y")

  # rescale coordinates
  nodes$x <- scale_safely(nodes$x)
  nodes$y <- scale_safely(nodes$y)

  # import vertex attributes
  for (i in igraph::list.vertex.attributes(model)) {
    nodes <- cbind(nodes, igraph::get.vertex.attribute(model, i), stringsAsFactors = stringsAsFactors)
    names(nodes)[ncol(nodes)] <- i
  }

  # edge list
  edges <- igraph::as_edgelist(model, names = FALSE)

  # edge list (if there are duplicated rows)
  if (
    nrow(edges[, 1:2, drop = FALSE]) > nrow(unique(edges[, 1:2, drop = FALSE]))
  ) {
    warning("duplicated edges detected")
  }

  edges <- data.frame(nodes[edges[, 1], 1:2], nodes[edges[, 2], 1:2])
  names(edges) <- c("x", "y", "xend", "yend")

  # arrow gap (thanks to @heike and @ethen8181 for their work on this issue)
  if (arrow.gap > 0) {
    x.length <- edges$xend - edges$x
    y.length <- edges$yend - edges$y
    arrow.gap <- arrow.gap / sqrt(x.length^2 + y.length^2)
    edges$xend <- edges$x + (1 - arrow.gap) * x.length
    edges$yend <- edges$y + (1 - arrow.gap) * y.length
  }

  # import edge attributes
  for (i in igraph::list.edge.attributes(model)) {
    edges <- cbind(edges, igraph::get.edge.attribute(model, i), stringsAsFactors = stringsAsFactors)
    names(edges)[ncol(edges)] <- i
  }

  if (nrow(edges) != 0) {
    # merge edges and nodes data
    edges <- merge(nodes, edges, by = c("x", "y"), all = TRUE)

    # add missing columns to nodes data
    nodes$xend <- nodes$x
    nodes$yend <- nodes$y
    names(nodes) <- names(edges)[1:ncol(nodes)]

    # make nodes data of identical dimensions to edges data
    missing.cols <- names(edges)[which(!(names(edges) %in% names(nodes)))]
    nodes[missing.cols] <- NA

    # panelize nodes (for temporal networks)
    if (!is.null(by)) {
      nodes <- lapply(sort(unique(edges[, by])), function(x) {
        y <- nodes
        y[, by] <- x
        y
      })
      nodes <- do.call(rbind, nodes)
    }

    # return a data frame with network.size(model) + network.edgecount(model) rows,
    # or length(unique(edges[, by ])) * network.size(model) + network.edgecount(model)
    # rows if the nodes have been panelized

    # [NOTE] `edges` has to be passed first to `rbind` in order for the edge
    # attributes (e.g. factor) to be preserved in the result; this should not
    # affect plotting the result, but differs from `ggnetwork` 0.5.1: `nodes`
    # is now at the end of the result rather at the beginning
    return(unique(rbind(edges[!is.na(edges$xend), ], nodes)))
  } else {
    # add missing columns to nodes data
    nodes$xend <- nodes$x
    nodes$yend <- nodes$y
    return(nodes)
  }
}
