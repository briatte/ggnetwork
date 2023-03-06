#' Fortify method for networks of class \code{\link[network]{network}}
#'
#' See the vignette at \url{https://briatte.github.io/ggnetwork/} for a
#' description of both this function and the rest of the \code{ggnetwork}
#' package.
#'
#' @param model an object of class \code{\link[network]{network}}.
#' @param data not used by this method.
#' @param layout a network layout supplied by \code{\link[sna]{gplot.layout}},
#'  such as \code{"fruchtermanreingold"} (the default), or a two-column matrix
#'   with as many rows as there are nodes in the network, in which case the
#'   matrix is used as nodes coordinates.
#' @inheritParams format_fortify
#' @param ... additional parameters for the \code{layout} argument; see
#'   \code{\link[sna]{gplot.layout}} for available options.
#'
#' @details \code{fortify.network} will return a warning if it finds duplicated
#'   edges after converting the network to an edge list. Duplicated edges should
#'   be eliminated in favour of single weighted edges before using a network
#'   layout that supports edge weights, such as the Kamada-Kawai force-directed
#'   placement algorithm.
#'
#' @return a \code{\link[base]{data.frame}} object.
#'
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
#'   ggplot(
#'     ggnetwork(emon[[1]], layout = "kamadakawai", arrow.gap = 0.025),
#'     aes(x, y, xend = xend, yend = yend)
#'   ) +
#'     geom_edges(aes(color = Frequency),
#'       arrow = arrow(length = unit(10, "pt"), type = "closed")
#'     ) +
#'     geom_nodes(aes(size = Formalization)) +
#'     scale_color_gradient(low = "grey50", high = "tomato") +
#'     scale_size_area(breaks = 1:3) +
#'     theme_blank()
#'
#'   # plot example with curved edges
#'   ggplot(
#'     ggnetwork(emon[[1]], layout = "kamadakawai", arrow.gap = 0.025),
#'     aes(x, y, xend = xend, yend = yend)
#'   ) +
#'     geom_edges(aes(color = Frequency),
#'       curvature = 0.1,
#'       arrow = arrow(length = unit(10, "pt"), type = "open")
#'     ) +
#'     geom_nodes(aes(size = Formalization)) +
#'     scale_color_gradient(low = "grey50", high = "tomato") +
#'     scale_size_area(breaks = 1:3) +
#'     theme_blank()
#'
#'   # facet by edge attribute
#'   ggplot(
#'     ggnetwork(emon[[1]], arrow.gap = 0.02, by = "Frequency"),
#'     aes(x, y, xend = xend, yend = yend)
#'   ) +
#'     geom_edges(arrow = arrow(length = unit(5, "pt"), type = "closed")) +
#'     geom_nodes() +
#'     theme_blank() +
#'     facet_grid(. ~ Frequency, labeller = label_both)
#'
#'   # user-provided layout
#'   ggplot(
#'     ggnetwork(emon[[1]], layout = matrix(runif(28), ncol = 2)),
#'     aes(x, y, xend = xend, yend = yend)
#'   ) +
#'     geom_edges(arrow = arrow(length = unit(5, "pt"), type = "closed")) +
#'     geom_nodes() +
#'     theme_blank()
#' }
#'
#' @export
fortify.network <- function(
  model,
  data = NULL,
  layout = "fruchtermanreingold",
  weights = NULL,
  arrow.gap = ifelse(network::is.directed(model), 0.025, 0),
  by = NULL,
  scale = TRUE,
  stringsAsFactors = getOption("stringsAsFactors", FALSE),
  ...
) {
  # node placement
  if (inherits(layout, "matrix") && identical(dim(layout), c(as.integer(network::network.size(model)), 2L))) {
    nodes <- layout[, 1:2 ]
  } else if (inherits(layout, "matrix")) {
    stop("layout matrix dimensions do not match network size")
  } else {
    layout <- eval(parse(text = paste0("sna::gplot.layout.", layout)))
    nodes <- do.call(layout, list(model, layout.par = list(...)))
  }

  format_fortify(
    model = model,
    nodes = nodes,
    weights = weights,
    arrow.gap = arrow.gap,
    by = by,
    scale = scale,
    stringsAsFactors = stringsAsFactors,
    .list_vertex_attributes_fun = network::list.vertex.attributes,
    .get_vertex_attributes_fun = network::get.vertex.attribute,
    .list_edges_attributes_fun = network::list.edge.attributes,
    .get_edges_attributes_fun = network::get.edge.attribute,
    .as_edges_list_fun = network::as.matrix.network.edgelist
  )
}
