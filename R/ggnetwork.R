if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("xend", "yend"))
}

#' Convert a network object to a data frame.
#'
#' See the vignette at \url{https://briatte.github.io/ggnetwork/} for a
#' description of both this function and the rest of the \code{ggnetwork}
#' package.
#' @param x an object of class \code{\link[network]{network}}, or any object
#' that can be coerced to this class, such as an adjacency or incidence matrix,
#' or an edge list: see \link[network]{edgeset.constructors} and
#' \link[network]{network} for details. If the object is of class
#' \code{\link[igraph:igraph-package]{igraph}} and the
#' \code{\link[intergraph:intergraph-package]{intergraph}} package is installed,
#' it will be used to convert the object: see
#' \code{\link[intergraph]{asNetwork}} for details.
#' @param layout a network layout supplied by \code{\link[sna]{gplot.layout}}
#' @param arrow.gap a parameter that will shorten the network edges in order to
#' avoid overplotting edge arrows and nodes; defaults to \code{0} when the
#' network is undirected (no edge shortening), or to \code{0.05} when the
#' network is directed. Small values near \code{0.05} will generally achieve
#' good results when the size of the nodes is reasonably small.
#' @param ... other parameters that will get passed to the network layout
#' algorithm as a list
#' @import sna
#' @examples
#' if (require(ggplot2) && require(network)) {
#'
#'   # source: ?network::flo
#'   data(flo)
#'
#'   # data example
#'   ggnetwork(flo)
#'
#'   # plot example
#'   ggplot(ggnetwork(flo), aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(alpha = 0.5) +
#'     geom_nodes(size = 12, color = "white") +
#'     geom_nodetext(aes(label = vertex.names), fontface = "bold") +
#'     theme_blank()
#'
#'   # source: ?network::emon
#'   data(emon)
#'
#'   # data example
#'   ggnetwork(emon[[1]], layout = "target", niter = 100)
#'
#'   # plot example
#'   ggplot(ggnetwork(emon[[1]], layout = "kamadakawai", arrow.gap = 0.05),
#'          aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(aes(color = Frequency),
#'                arrow = arrow(length = unit(10, "pt"), type = "closed")) +
#'     geom_nodes(aes(size = Formalization)) +
#'     scale_color_gradient(low = "grey50", high = "tomato") +
#'     scale_size_area(breaks = 1:3) +
#'     theme_blank()
#'
#' }
#' @export
ggnetwork <- function(x, layout = "fruchtermanreingold",
                      arrow.gap = ifelse(network::is.directed(x), 0.05, 0),
                      ...) {
  load_pkg("sna")

  if (class(x) == "igraph" &&
      "intergraph" %in% rownames(installed.packages())) {
    x = intergraph::asNetwork(x)
  } else if (class(x) == "igraph") {
    stop("install the 'intergraph' package to use igraph objects with ggnetwork")
  }

  if (!network::is.network(x)) {
    x = try(network::network(x), silent = TRUE)
  }

  if (!network::is.network(x)) {
    stop("could not coerce object to a network")
  }

  # node placement
  layout = paste0("gplot.layout.", layout)
  nodes = try(do.call(layout, list(x, layout.par = list(...))), silent = TRUE)

  if (!exists(layout)) {
    stop("unsupported layout")
  }

  # store coordinates
  nodes = data.frame(nodes)
  names(nodes) = c("x", "y")

  # rescale coordinates
  nodes$x = scale(nodes$x, center = min(nodes$x), scale = diff(range(nodes$x)))
  nodes$y = scale(nodes$y, center = min(nodes$y), scale = diff(range(nodes$y)))

  # import vertex attributes
  for (y in network::list.vertex.attributes(x)) {
    nodes = cbind(nodes, network::get.vertex.attribute(x, y))
    names(nodes)[ncol(nodes)] = y
  }

  # edge list
  edges = network::as.matrix.network.edgelist(x)

  edges = data.frame(nodes[edges[, 1], 1:2], nodes[edges[, 2], 1:2])
  names(edges) = c("x", "y", "xend", "yend")

  # arrow gap (thanks to @heike and @ethen8181 for their work on this issue)
  if (arrow.gap > 0) {
    x.length = with(edges, xend - x)
    y.length = with(edges, yend - y)
    arrow.gap = with(edges, arrow.gap / sqrt(x.length ^ 2 + y.length ^ 2))
    edges = transform(
      edges,
#       x = x + arrow.gap * x.length,
#       y = y + arrow.gap * y.length,
      xend = x + (1 - arrow.gap) * x.length,
      yend = y + (1 - arrow.gap) * y.length
    )
  }
  print("dude")

  # import edge attributes
  for (y in network::list.edge.attributes(x)) {
    edges = cbind(edges, network::get.edge.attribute(x, y))
    names(edges)[ncol(edges)] = y
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

  # return a data frame with network.size(x) + network.edgecount(x) rows
  unique(rbind(nodes, edges[!is.na(edges$xend),]))
}
