#' Draw the nodes of a network.
#'
#' All arguments to this geom are identical to those of
#' \code{\link[ggplot2]{geom_point}}.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @examples
#' if (require(network) && require(sna)) {
#'   data(flo, package = "network")
#'   n <- network(flo, directed = FALSE)
#'
#'   # just nodes
#'   ggplot(n, aes(x, y)) +
#'     geom_nodes(size = 3, shape = 21, colour = "steelblue") +
#'     theme_blank()
#'
#'   # with edges
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(colour = "steelblue") +
#'     geom_nodes(size = 3, shape = 21, colour = "steelblue", fill = "white") +
#'     theme_blank()
#'
#'   # with nodes sized according to degree centrality
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(colour = "steelblue") +
#'     geom_nodes(size = degree(n), shape = 21, colour = "steelblue", fill = "white") +
#'     theme_blank()
#'
#'   # with nodes colored according to betweenness centrality
#'
#'   n %v% "betweenness" <- betweenness(flo)
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(colour = "grey50") +
#'     geom_nodes(aes(colour = betweenness), size = 3) +
#'     scale_colour_gradient(low = "gold", high = "tomato") +
#'     theme_blank() +
#'     theme(legend.position = "bottom")
#' }
#'
#' @export
geom_nodes <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodes,
    geom = ggplot2::GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

# note: the nudge_x,nudge_y arguments below are duplicated from the original
# function due to a small bug in roxygen2 5.0.1 (see issue #444 and PR #445)

#' Label the nodes of a network.
#'
#' All arguments to these geoms are identical to those of
#' \code{\link[ggplot2]{geom_text}} and \code{\link[ggplot2]{geom_label}}.
#'
#' @inheritParams ggplot2::geom_text
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#'
#' @examples
#' ## geom_nodetext examples
#'
#' if (require(network) && require(sna)) {
#'   n <- network(rgraph(10, tprob = 0.2), directed = FALSE)
#'
#'   # just node labels
#'   ggplot(n, aes(x, y)) +
#'     geom_nodetext(aes(label = vertex.names)) +
#'     theme_blank()
#'
#'   # with nodes underneath
#'   ggplot(n, aes(x, y)) +
#'     geom_nodes(colour = "gold", size = 9) +
#'     geom_nodetext(aes(label = vertex.names)) +
#'     theme_blank()
#'
#'   # with nodes and edges
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(colour = "gold") +
#'     geom_nodes(colour = "gold", size = 9) +
#'     geom_nodetext(aes(label = vertex.names)) +
#'     theme_blank()
#' }
#'
#' @export
geom_nodetext <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0,
  nudge_y = 0,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodes,
    geom = ggplot2::GeomText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      parse = parse,
      check_overlap = check_overlap,
      ...
    )
  )
}

# note: the nudge_x,nudge_y arguments below are duplicated from the original
# function due to a small bug in roxygen2 5.0.1 (see issue #444 and PR #445)

#' Draw repulsive node labels
#'
#' All arguments to these geoms are identical to those of
#' \code{\link[ggrepel]{geom_text_repel}} and
#' \code{\link[ggrepel]{geom_label_repel}}.
#'
#' @inheritParams ggrepel::geom_text_repel
#' @param nudge_x,nudge_y Horizontal and vertical adjustments to nudge the
#'   starting position of each text label.
#'
#' @examples
#' ## geom_nodetext_repel example
#'
#' if (require(network) && require(sna)) {
#'   n <- network(rgraph(10, tprob = 0.2), directed = FALSE)
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(colour = "steelblue") +
#'     geom_nodetext_repel(aes(label = paste("node", vertex.names)),
#'       box.padding = unit(1, "lines")
#'     ) +
#'     geom_nodes(colour = "steelblue", size = 3) +
#'     theme_blank()
#' }
#'
#' @export
geom_nodetext_repel <- function(
  mapping = NULL,
  data = NULL,
  # stat = "identity",
  parse = FALSE,
  ...,
  box.padding = unit(0.25, "lines"),
  point.padding = unit(1e-06, "lines"),
  segment.colour = "#666666",
  segment.size = 0.5,
  arrow = NULL,
  force = 1,
  max.iter = 2000,
  nudge_x = 0,
  nudge_y = 0,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodes,
    geom = ggrepel::GeomTextRepel,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      box.padding = box.padding,
      point.padding = point.padding,
      segment.colour = segment.colour,
      segment.size = segment.size,
      arrow = arrow,
      force = force,
      max.iter = max.iter,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      ...
    )
  )
}

#' @rdname geom_nodetext
#'
#' @inheritParams ggplot2::geom_label
#'
#' @examples
#'
#' ## geom_nodelabel examples
#'
#' if (require(network) && require(sna)) {
#'   data(flo, package = "network")
#'   n <- network(flo, directed = FALSE)
#'
#'   # with text labels
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(colour = "grey50") +
#'     geom_nodelabel(aes(label = vertex.names)) +
#'     theme_blank()
#'
#'   # with text labels coloured according to degree centrality
#'   n %v% "degree" <- degree(n)
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(colour = "grey50") +
#'     geom_nodelabel(aes(label = vertex.names, fill = degree)) +
#'     scale_fill_gradient(low = "gold", high = "tomato") +
#'     theme_blank()
#'
#'   # label only a subset of all nodes
#'   high_degree <- function(x) {
#'     x[ x$degree > median(x$degree), ]
#'   }
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(colour = "steelblue") +
#'     geom_nodes(aes(size = degree), colour = "steelblue") +
#'     geom_nodelabel(aes(label = vertex.names),
#'       data = high_degree,
#'       colour = "white", fill = "tomato"
#'     ) +
#'     theme_blank()
#' }
#'
#' @export
geom_nodelabel <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0,
  nudge_y = 0,
  label.padding = unit(0.25, "lines"),
  label.r = unit(0.15, "lines"),
  label.size = 0.25,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodes,
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

#' @rdname geom_nodetext_repel
#'
#' @inheritParams ggrepel::geom_label_repel
#'
#' @examples
#' ## geom_nodelabel_repel examples
#'
#' if (require(network) && require(sna)) {
#'   data(flo, package = "network")
#'   n <- network(flo, directed = FALSE)
#'
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(colour = "steelblue") +
#'     geom_nodelabel_repel(aes(label = vertex.names),
#'       box.padding = unit(1, "lines")
#'     ) +
#'     geom_nodes(colour = "steelblue", size = 3) +
#'     theme_blank()
#'
#'   # label only a subset of all nodes
#'   n %v% "degree" <- degree(n)
#'   low_degree <- function(x) {
#'     x[ x$degree < median(x$degree), ]
#'   }
#'   ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#'     geom_edges(colour = "steelblue") +
#'     geom_nodelabel_repel(aes(label = vertex.names),
#'       box.padding = unit(1.5, "lines"),
#'       data = low_degree,
#'       segment.colour = "tomato",
#'       colour = "white", fill = "tomato"
#'     ) +
#'     geom_nodes(aes(size = degree), colour = "steelblue") +
#'     theme_blank()
#' }
#'
#' @export
geom_nodelabel_repel <- function(
  mapping = NULL,
  data = NULL,
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
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodes,
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

#' @keywords internal
StatNodes <- ggplot2::ggproto("StatNodes", ggplot2::Stat,
  compute_layer = function(data, scales, params) {
    if (all(c("xend", "yend") %in% names(data))) {
      unique(subset(data, select = c(-xend, -yend)))
    } else {
      unique(data)
    }
  }
)
