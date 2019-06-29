if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("xend", "yend"))
}

#' Fortify method for networks of class \code{\link[network]{network}}
#'
#' See the vignette at \url{https://briatte.github.io/ggnetwork/} for a
#' description of both this function and the rest of the \code{ggnetwork}
#' package.
#' @param model an object of class \code{\link[network]{network}}.
#' @param data not used by this method.
#' @param layout a network layout supplied by \code{\link[sna]{gplot.layout}},
#' such as \code{"fruchtermanreingold"} (the default), or a two-column matrix
#' with as many rows as there are nodes in the network, in which case the
#' matrix is used as nodes coordinates.
#' @param weights the name of an edge attribute to use as edge weights when
#' computing the network layout, if the layout supports such weights (see
#' 'Details').
#' Defaults to \code{NULL} (no edge weights).
#' @param arrow.gap a parameter that will shorten the network edges in order to
#' avoid overplotting edge arrows and nodes; defaults to \code{0} when the
#' network is undirected (no edge shortening), or to \code{0.025} when the
#' network is directed. Small values near \code{0.025} will generally achieve
#' good results when the size of the nodes is reasonably small.
#' @param by a character vector that matches an edge attribute, which will be
#' used to generate a data frame that can be plotted with
#' \code{\link[ggplot2]{facet_wrap}} or \code{\link[ggplot2]{facet_grid}}. The
#' nodes of the network will appear in all facets, at the same coordinates.
#' Defaults to \code{NULL} (no faceting).
#' @return a data.frame object.
#' @param ... additional parameters for the \code{layout} argument; see
#' \code{\link[sna]{gplot.layout}} for available options.
#' @details \code{fortify.network} will return a warning if it finds duplicated
#' edges after converting the network to an edge list. Duplicated edges should
#' be eliminated in favour of single weighted edges before using a network
#' layout that supports edge weights, such as the Kamada-Kawai force-directed
#' placement algorithm.
#' @import sna
#' @importFrom ggplot2 fortify
#' @method fortify network
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
#'   # data example with edge weights
#'   ggnetwork(emon[[1]], layout = "kamadakawai", weights = "Frequency")
#'
#'   # plot example with straight edges
#'   ggplot(ggnetwork(emon[[1]], layout = "kamadakawai", arrow.gap = 0.025),
#'          aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(aes(color = Frequency),
#'                arrow = arrow(length = unit(10, "pt"), type = "closed")) +
#'     geom_nodes(aes(size = Formalization)) +
#'     scale_color_gradient(low = "grey50", high = "tomato") +
#'     scale_size_area(breaks = 1:3) +
#'     theme_blank()
#'
#'   # plot example with curved edges
#'   ggplot(ggnetwork(emon[[1]], layout = "kamadakawai", arrow.gap = 0.025),
#'          aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(aes(color = Frequency), curvature = 0.1,
#'                arrow = arrow(length = unit(10, "pt"), type = "open")) +
#'     geom_nodes(aes(size = Formalization)) +
#'     scale_color_gradient(low = "grey50", high = "tomato") +
#'     scale_size_area(breaks = 1:3) +
#'     theme_blank()
#'
#'   # facet by edge attribute
#'   ggplot(ggnetwork(emon[[1]], arrow.gap = 0.02, by = "Frequency"),
#'          aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(arrow = arrow(length = unit(5, "pt"), type = "closed")) +
#'     geom_nodes() +
#'     theme_blank() +
#'     facet_grid(. ~ Frequency, labeller = label_both)
#'
#'   # user-provided layout
#'   ggplot(ggnetwork(emon[[1]], layout = matrix(runif(28), ncol = 2)),
#'          aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(arrow = arrow(length = unit(5, "pt"), type = "closed")) +
#'     geom_nodes() +
#'     theme_blank()
#'
#' }
#' @export
fortify.network <- function(model, data = NULL,
                            layout = "fruchtermanreingold", weights = NULL,
                            arrow.gap = ifelse(network::is.directed(model), 0.025, 0),
                            by = NULL,
                            ...) {
  x = model

  # node placement
  if (class(layout) == "matrix" &&
      nrow(layout) == network::network.size(x) &&
      ncol(layout) == 2) {
    nodes = layout[, 1:2 ]
  } else {
    layout = paste0("gplot.layout.", layout)
    ns <- loadNamespace("sna")
    if (!exists(layout, envir=ns, inherits=FALSE)) {
      stop("unsupported layout")
    }
    nodes = do.call( utils::getFromNamespace(layout, ns), list(x, layout.par = list(...)))
  }

  # store coordinates
  nodes = data.frame(nodes)
  names(nodes) = c("x", "y")

  # rescale coordinates
  nodes$x <- scale_safely(nodes$x)
  nodes$y <- scale_safely(nodes$y)

  # import vertex attributes
  for (y in network::list.vertex.attributes(x)) {
    nodes = cbind(nodes, network::get.vertex.attribute(x, y))
    names(nodes)[ncol(nodes)] = y
  }

  # edge list
  edges = network::as.matrix.network.edgelist(x, attrname = weights)

  # edge list (if there are duplicated rows)
  if (nrow(edges[, 1:2, drop = FALSE]) > nrow(unique(edges[, 1:2, drop = FALSE]))) {
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
  for (y in network::list.edge.attributes(x)) {
    edges = cbind(edges, network::get.edge.attribute(x, y))
    names(edges)[ncol(edges)] = y
  }

  if (nrow(edges)!=0) {
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
    return(unique(rbind(nodes, edges[ !is.na(edges$xend), ])))
  } else {
    # add missing columns to nodes data
    nodes$xend = nodes$x
    nodes$yend = nodes$y
    return(nodes)
  }

}
