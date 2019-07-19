#' Draw the edges of a network.
#'
#' All arguments to this geom are identical to those of
#' \code{\link[ggplot2]{geom_segment}}, including \code{arrow}, which is useful
#' to plot directed networks in conjunction with the \code{arrow.gap} argument
#' of \code{\link{fortify.network}}. The \code{curvature}, \code{angle} and
#' \code{ncp} arguments of \code{\link[ggplot2]{geom_curve}} are also available:
#' if \code{curvature} is set to any value above \code{0} (the default), the
#' edges produced by \code{geom_edges} will be curved.
#' @inheritParams ggplot2::geom_segment
#' @inheritParams ggplot2::geom_curve
#' @importFrom ggplot2 GeomSegment GeomCurve layer
#' @examples
#' if (require(network) && require(sna)) {
#'
#'   # rerun if the example does not produce reciprocated ties
#'   n <- network(rgraph(10, tprob = 0.2), directed = TRUE)
#'
#'   # just edges
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(size = 1, colour = "steelblue") +
#'     theme_blank()
#'
#'   # with nodes
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(size = 1, colour = "steelblue") +
#'     geom_nodes(size = 3, colour = "steelblue") +
#'     theme_blank()
#'
#'   # with arrows
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(
#'       size = 1, colour = "steelblue",
#'       arrow = arrow(length = unit(0.5, "lines"), type = "closed")
#'     ) +
#'     geom_nodes(size = 3, colour = "steelblue") +
#'     theme_blank()
#'
#'   # with curvature
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(
#'       size = 1, colour = "steelblue", curvature = 0.15,
#'       arrow = arrow(length = unit(0.5, "lines"), type = "closed")
#'     ) +
#'     geom_nodes(size = 3, colour = "steelblue") +
#'     theme_blank()
#'
#'   # arbitrary categorical edge attribute
#'   e <- sample(letters[ 1:2 ], network.edgecount(n), replace = TRUE)
#'   set.edge.attribute(n, "type", e)
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(aes(linetype = type),
#'       size = 1, curvature = 0.15,
#'       arrow = arrow(length = unit(0.5, "lines"), type = "closed")
#'     ) +
#'     geom_nodes(size = 3, colour = "steelblue") +
#'     theme_blank()
#'
#'   # arbitrary numeric edge attribute (signed network)
#'   e <- sample(-2:2, network.edgecount(n), replace = TRUE)
#'   set.edge.attribute(n, "weight", e)
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(aes(colour = weight),
#'       curvature = 0.15,
#'       arrow = arrow(length = unit(0.5, "lines"), type = "closed")
#'     ) +
#'     geom_nodes(size = 3, colour = "grey50") +
#'     scale_colour_gradient(low = "steelblue", high = "tomato") +
#'     theme_blank()
#'
#'   # draw only a subset of all edges
#'   positive_weight <- function(x) {
#'     x[ x$weight >= 0, ]
#'   }
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(aes(colour = weight), data = positive_weight) +
#'     geom_nodes(size = 4, colour = "grey50") +
#'     scale_colour_gradient(low = "gold", high = "tomato") +
#'     theme_blank()
#' }
#' @export
geom_edges <- function(mapping = NULL, data = NULL,
                       position = "identity", arrow = NULL,
                       curvature = 0, angle = 90, ncp = 5,
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                       ...) {
  if (!curvature) {
    geom <- ggplot2::GeomSegment
    params <- list(
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  } else {
    geom <- ggplot2::GeomCurve
    params <- list(
      arrow = arrow,
      curvature = curvature,
      angle = angle,
      ncp = ncp,
      na.rm = na.rm,
      ...
    )
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatEdges,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

# note: the nudge_x,nudge_y arguments below are duplicated from the original
# function due to a small bug in roxygen2 5.0.1 (see issue #444 and PR #445)

#' Label the edges of a network.
#'
#' All arguments to both \code{\link{geom_edgetext}} and
#' \code{\link{geom_edgelabel}} are identical to those of
#' \code{\link[ggplot2]{geom_label}}, with the only difference that the
#' \code{label.size} argument defaults to \code{0} in order to avoid drawing a
#' border around the edge labels. The labels will be drawn at mid-edges.
#' \code{\link{geom_text}} and \code{\link{geom_label}} produce strictly
#' identical results.
#' @inheritParams ggplot2::geom_label
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#' @importFrom ggplot2 unit position_nudge layer GeomLabel
#' @examples
#' if (require(network) && require(sna)) {
#'   data(flo, package = "network")
#'   n <- network(flo, directed = FALSE)
#'
#'   # arbitrary categorical edge attribute
#'   e <- sample(letters[ 1:4 ], network.edgecount(n), replace = TRUE)
#'   set.edge.attribute(n, "type", e)
#'
#'   # with labelled edges
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(aes(colour = type)) +
#'     geom_edgetext(aes(label = type, colour = type)) +
#'     geom_nodes(size = 4, colour = "grey50") +
#'     theme_blank()
#'
#'   # label only a subset of all edges with arbitrary symbol
#'   edge_type <- function(x) {
#'     x[ x$type == "a", ]
#'   }
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges() +
#'     geom_edgetext(label = "=", data = edge_type) +
#'     geom_nodes(size = 4, colour = "grey50") +
#'     theme_blank()
#' }
#' @export
geom_edgetext <- function(mapping = NULL, data = NULL,
                          position = "identity", parse = FALSE, ...,
                          nudge_x = 0, nudge_y = 0,
                          label.padding = unit(0.25, "lines"),
                          label.r = ggplot2::unit(0.15, "lines"),
                          label.size = 0,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }

    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatMidEdges,
    geom = ggplot2::GeomLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      ...
    )
  )
}

# note: the nudge_x,nudge_y arguments below are duplicated from the original
# function due to a small bug in roxygen2 5.0.1 (see issue #444 and PR #445)

#' Draw repulsive edge labels.
#'
#' All arguments to both \code{\link{geom_edgetext_repel}} and
#' \code{\link{geom_edgelabel_repel}} are identical to those of
#' \code{\link[ggrepel]{geom_label_repel}}. \code{\link{geom_text_repel}} and
#' \code{\link{geom_label_repel}} produce strictly identical results.
#' @inheritParams ggrepel::geom_label_repel
#' @param nudge_x,nudge_y Horizontal and vertical adjustments to nudge the
#'   starting position of each text label.
#' @importFrom ggplot2 layer
#' @importFrom ggrepel GeomLabelRepel
#' @examples
#' if (require(network) && require(sna)) {
#'   data(flo, package = "network")
#'   n <- network(flo, directed = FALSE)
#'
#'   # arbitrary categorical edge attribute
#'   e <- sample(1:4, network.edgecount(n), replace = TRUE)
#'   set.edge.attribute(n, "day", e)
#'
#'   # with repulsive edge labels
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges() +
#'     geom_edgetext_repel(aes(label = day), box.padding = unit(0.5, "lines")) +
#'     geom_nodes(size = 4, colour = "grey50") +
#'     theme_blank()
#'
#'   # repulsive edge labels for only a subset of all edges
#'   edge_day <- function(x) {
#'     x[ x$day > 2, ]
#'   }
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(aes(colour = cut(day, (4:0)[ -3 ]))) +
#'     geom_edgetext_repel(aes(
#'       label = paste("day", day),
#'       colour = cut(day, (4:0)[ -3 ])
#'     ), data = edge_day) +
#'     geom_nodes(size = 4, colour = "grey50") +
#'     scale_colour_manual("day",
#'       labels = c("old ties", "day 3", "day 4"),
#'       values = c("grey50", "gold", "tomato")
#'     ) +
#'     theme_blank()
#' }
#' @export
geom_edgetext_repel <- function(
                                mapping = NULL, data = NULL,
                                parse = FALSE,
                                ...,
                                box.padding = unit(0.25, "lines"),
                                label.padding = unit(0.25, "lines"),
                                point.padding = unit(1e-6, "lines"),
                                label.r = unit(0.15, "lines"),
                                label.size = 0.25,
                                segment.colour = "#666666",
                                segment.size = 0.5,
                                arrow = NULL,
                                force = 1,
                                max.iter = 2000,
                                nudge_x = 0,
                                nudge_y = 0,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatMidEdges,
    geom = ggrepel::GeomLabelRepel,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      box.padding = box.padding,
      label.padding = label.padding,
      point.padding = point.padding,
      label.r = label.r,
      label.size = label.size,
      segment.colour = segment.colour,
      segment.size = segment.size,
      arrow = arrow,
      na.rm = na.rm,
      force = force,
      max.iter = max.iter,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      ...
    )
  )
}

#' @rdname geom_edgetext
#' @export
geom_edgelabel <- geom_edgetext

#' @rdname geom_edgetext_repel
#' @export
geom_edgelabel_repel <- geom_edgetext_repel

#' @importFrom ggplot2 ggproto
#' @keywords internal
StatEdges <-
  ggplot2::ggproto("StatEdges", ggplot2::Stat,
    compute_layer = function(data, scales, params) {
      unique(subset(data, !(x == xend & y == yend)))
    }
  )

#' @importFrom ggplot2 ggproto
#' @keywords internal
StatMidEdges <-
  ggplot2::ggproto("StatMidEdges", ggplot2::Stat,
    compute_layer = function(data, scales, params) {
      data <- subset(data, !(x == xend & y == yend))
      data$x <- (data$x + data$xend) / 2
      data$y <- (data$y + data$yend) / 2
      unique(subset(data, select = c(-xend, -yend)))
    }
  )
