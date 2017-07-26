#' Fortify method for networks of class \code{\link[igraph:igraph-package]{igraph}}
#'
#' This function requires the \code{\link[intergraph:intergraph-package]{intergraph}}
#' package to be installed.
#' @param model an object of class \code{\link[igraph:igraph-package]{igraph}}
#' @param layout a function call to an 
#' \code{\link[igraph:igraph-package]{igraph}} layout function, or a 2 column
#' matrix giving the x and y coordinates for the vertices.  See 
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

  
  x = model
  
  # node placement
  if (class(layout) == "matrix" &&
      nrow(layout) == igraph::gorder(x) &&
      ncol(layout) == 2) {
    nodes = layout[, 1:2 ]
  } else {
    nodes <- igraph::layout_(x, layout)
  }
  
  # store coordinates
  nodes = data.frame(nodes)
  names(nodes) = c("x", "y")
  
  # rescale coordinates
  nodes$x = scale(nodes$x, center = min(nodes$x), scale = diff(range(nodes$x)))
  nodes$y = scale(nodes$y, center = min(nodes$y), scale = diff(range(nodes$y)))
  
  # import vertex attributes
  if (length(igraph::list.vertex.attributes(x))) {
    nodes <- cbind(
      nodes,
      vapply(
        igraph::list.vertex.attributes(x),
        FUN = igraph::get.vertex.attribute,
        graph = x,
        FUN.VALUE = rep(1, igraph::gorder(x)),
        USE.NAMES = T
      )
  }

  
  # edge list
  edges <- igraph::as_edgelist(x, names = F)
  
  # edge list (if there are duplicated rows)
  if (nrow(edges[, 1:2]) > nrow(unique(edges[, 1:2]))) {
    warning("duplicated edges detected")
  }
  
  edges = data.frame(nodes[edges[, 1], 1:2], nodes[edges[, 2], 1:2])
  names(edges) = c("x", "y", "xend", "yend")
  
  # arrow gap (thanks to @heike and @ethen8181 for their work on this issue)
  if (arrow.gap > 0) {
    x.length = with(edges, xend - x)
    y.length = with(edges, yend - y)
    arrow.gap = with(edges, arrow.gap / sqrt(x.length ^ 2 + y.length ^ 2))
    edges = transform(
      edges,
      # x = x + arrow.gap * x.length,
      # y = y + arrow.gap * y.length,
      xend = x + (1 - arrow.gap) * x.length,
      yend = y + (1 - arrow.gap) * y.length
    )
  }
  
  # import edge attributes
  if (length(igraph::list.edge.attributes(x))) {
    edges <- cbind(
      edges,
      vapply(
        igraph::list.edge.attributes(x),
        FUN = igraph::get.edge.attribute,
        graph = x,
        FUN.VALUE = rep(1, igraph::gsize(x)),
        USE.NAMES = T
      )
    )
  }

  # merge edges and nodes data
  edges = merge(nodes, edges, by = c("x", "y"), all = TRUE)
  
  # add missing columns to nodes data
  nodes$xend = nodes$x
  nodes$yend = nodes$y
  names(nodes) = names(edges)[1:ncol(nodes)]
  
  # make nodes data of identical dimensions to edges data
  for (y in names(edges)[(1 + ncol(nodes)):ncol(edges)]) {
    nodes = cbind(nodes, NA)
    names(nodes)[ncol(nodes)] = y
  }
  
  # panelize nodes (for temporal networks)
  if (!is.null(by)) {
    nodes = lapply(sort(unique(edges[, by ])), function(x) {
      y = nodes
      y[, by ] = x
      y
    })
    nodes = do.call(rbind, nodes)
  }
  
  # return a data frame with network.size(x) + network.edgecount(x) rows,
  # or length(unique(edges[, by ])) * network.size(x) + network.edgecount(x)
  # rows if the nodes have been panelized
  unique(rbind(nodes, edges[ !is.na(edges$xend), ]))
 
}
