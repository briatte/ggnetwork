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
#' @param ... additional parameters for the \code{\link[igraph]{layout_}} function
#'
#' @export
fortify.igraph <- function(
  model,
  data = NULL,
  layout = igraph::nicely(),
  arrow.gap = ifelse(igraph::is.directed(model), 0.025, 0),
  by = NULL,
  ...
) {
  # node placement
  if (class(layout) == "matrix" && identical(dim(layout), c(igraph::gorder(model), 2L))) {
    nodes <- layout[, 1:2 ]
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
  if (length(igraph::list.vertex.attributes(model))) {
    nodes <- cbind(
      nodes,
      sapply(
        igraph::list.vertex.attributes(model),
        FUN = igraph::get.vertex.attribute,
        graph = model,
        USE.NAMES = TRUE
      )
    )
  }


  # edge list
  edges <- igraph::as_edgelist(model, names = FALSE)

  # edge list (if there are duplicated rows)
  if (nrow(edges[, 1:2]) > nrow(unique(edges[, 1:2]))) {
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
  if (length(igraph::list.edge.attributes(model))) {
    edges <- cbind(
      edges,
      sapply(
        igraph::list.edge.attributes(model),
        FUN = igraph::get.edge.attribute,
        graph = model,
        USE.NAMES = TRUE
      )
    )
  }

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
  # or length(unique(edges[, by])) * network.size(model) + network.edgecount(model)
  # rows if the nodes have been panelized
  unique(rbind(nodes, edges[!is.na(edges$xend), ]))
}
