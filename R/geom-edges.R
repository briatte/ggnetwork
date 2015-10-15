#' Draw the edges of a network.
#'
#' All arguments to this geom are identical to those of
#' \code{\link[ggplot2]{geom_segment}}, including \code{arrow}, which is useful
#' to plot directed networks in conjunction with the \code{arrow.gap} argument
#' of \code{\link{ggnetwork}}. The \code{curvature}, \code{angle} and \code{ncp}
#' arguments from \code{\link[ggplot2]{geom_curve}} are also available: if
#' \code{curvature} is set to any value above \code{0} (the default), the edges
#' produced by \code{geom_edges} will be curved.
#' @param mapping see \code{\link[ggplot2]{geom_segment}}
#' @param data see \code{\link[ggplot2]{geom_segment}}
#' @param position see \code{\link[ggplot2]{geom_segment}}
#' @param arrow see \code{\link[ggplot2]{geom_segment}}
#' @param curvature see \code{\link[ggplot2]{geom_curve}}
#' @param angle see \code{\link[ggplot2]{geom_curve}}
#' @param ncp see \code{\link[ggplot2]{geom_curve}}
#' @param na.rm see \code{\link[ggplot2]{geom_segment}}
#' @param show.legend see \code{\link[ggplot2]{geom_segment}}
#' @param inherit.aes see \code{\link[ggplot2]{geom_segment}}
#' @param ... see \code{\link[ggplot2]{geom_segment}}
#' @importFrom ggplot2 GeomSegment GeomCurve layer
#' @export
geom_edges <- function(mapping = NULL, data = NULL,
                       position = "identity", arrow = NULL,
                       curvature = 0, angle = 90, ncp = 5,
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                       ...) {

  if (!curvature) {
    geom = ggplot2::GeomSegment
    params = list(
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  } else {
    geom = ggplot2::GeomCurve
    params = list(
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

#' Label the edges of a network.
#'
#' All arguments to this geom are identical to those of
#' \code{\link[ggplot2]{geom_label}}, with the only difference that the
#' \code{label.size} argument defaults to \code{0} in order to avoid drawing a
#' border around the edge labels. The labels will be drawn at mid-edges.
#' @param mapping see \code{\link[ggplot2]{geom_label}}
#' @param data see \code{\link[ggplot2]{geom_label}}
#' @param position see \code{\link[ggplot2]{geom_label}}
#' @param parse see \code{\link[ggplot2]{geom_label}}
#' @param ... see \code{\link[ggplot2]{geom_label}}
#' @param nudge_x see \code{\link[ggplot2]{geom_label}}
#' @param nudge_y see \code{\link[ggplot2]{geom_label}}
#' @param label.padding see \code{\link[ggplot2]{geom_label}}
#' @param label.r see \code{\link[ggplot2]{geom_label}}
#' @param label.size see \code{\link[ggplot2]{geom_label}}
#' @param na.rm see \code{\link[ggplot2]{geom_label}}
#' @param show.legend see \code{\link[ggplot2]{geom_label}}
#' @param inherit.aes see \code{\link[ggplot2]{geom_label}}
#' @importFrom ggplot2 unit position_nudge layer GeomLabel
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

#' @importFrom ggplot2 ggproto
#' @keywords internal
StatEdges <-
  ggplot2::ggproto("StatEdges", Stat,
                   compute_layer = function(data, scales, params) {
                     unique(subset(data, !(x == xend & y == yend)))
                   }
  )

#' @importFrom ggplot2 ggproto
#' @keywords internal
StatMidEdges <-
  ggplot2::ggproto("StatMidEdges", Stat,
                   compute_layer = function(data, scales, params) {
                     data = subset(data, x != xend & y != yend)
                     data$x = (data$x + data$xend) / 2
                     data$y = (data$y + data$yend) / 2
                     unique(subset(data, select = c(-xend, -yend)))
                   }
  )
