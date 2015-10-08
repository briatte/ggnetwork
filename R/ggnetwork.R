if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("xend", "yend"))
}

#' Convert a network object to a data frame
#'
#' See the vignette for a description of both this function and the rest of the
#' \code{ggnetwork} package.
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
#' network is undirected (no edge shortening), or to \code{0.025} when the
#' network is directed.
#' @param ... other parameters passed to the network layout function as a list
#' @examples
#' if (require(network)) {
#'   data(emon)
#'   ggnetwork(emon[[1]])
#'   ggnetwork(emon[[1]], layout = "target", niter = 100)
#' }
#' @export
ggnetwork <- function(x, layout = "fruchtermanreingold",
                      arrow.gap = ifelse(network::is.directed(x), 0.025, 0),
                      ...) {

  load_pkg("sna")

  if (class(x) == "igraph" && "intergraph" %in% rownames(installed.packages())) {
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
    names(nodes)[ ncol(nodes) ] = y
  }

  # edge list
  edges = network::as.matrix.network.edgelist(x)

  edges = data.frame(nodes[ edges[, 1], 1:2 ], nodes[ edges[, 2], 1:2 ])
  names(edges) = c("x", "y", "xend", "yend")
  # arrow gap (thanks to @heike)
  if (arrow.gap > 0) {

    arrow.gap = with(edges, arrow.gap / sqrt((xend - x) ^ 2 + (yend - y) ^ 2))
    edges = transform(edges,
                       xend = x + (1 - arrow.gap) * (xend - x),
                       yend = y + (1 - arrow.gap) * (yend - y))

  }

  # import edge attributes
  for (y in network::list.edge.attributes(x)) {
    edges = cbind(edges, network::get.edge.attribute(x, y))
    names(edges)[ ncol(edges) ] = y
  }

  # merge edges and nodes data
  edges = merge(nodes, edges, by = c("x", "y"), all = TRUE)

  # add missing columns to nodes data
  nodes$xend = nodes$x
  nodes$yend = nodes$y
  names(nodes) = names(edges)[ 1:ncol(nodes) ]

  # make nodes data of identical dimensions to edges data
  for (y in names(edges)[ (1 + ncol(nodes)):ncol(edges) ]) {
    nodes = cbind(nodes, NA)
    names(nodes)[ ncol(nodes) ] = y
  }

  # return a data frame with network.size(x) + network.edgecount(x) rows
  unique(rbind(nodes, edges[ !is.na(edges$xend), ]))
}
