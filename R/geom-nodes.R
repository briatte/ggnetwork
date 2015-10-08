#' Draw the nodes of a network.
#'
#' All arguments to this function are identical to those of
#' \code{\link[ggplot2]{geom_point}}.
#' @param mapping see \code{\link[ggplot2]{geom_point}}
#' @param data see \code{\link[ggplot2]{geom_point}}
#' @param stat see \code{\link[ggplot2]{geom_point}}
#' @param position see \code{\link[ggplot2]{geom_point}}
#' @param na.rm see \code{\link[ggplot2]{geom_point}}
#' @param show.legend see \code{\link[ggplot2]{geom_point}}
#' @param inherit.aes see \code{\link[ggplot2]{geom_point}}
#' @param ... see \code{\link[ggplot2]{geom_point}}
#' @importFrom ggplot2 layer ggproto
#' @export
geom_nodes <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE, ...) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomNodes,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )

}

#' @rdname geom_nodes
#' @format NULL
#' @usage NULL
#' @export
GeomNodes <-
  ggproto("GeomNodes", Geom,
          required_aes = c("x", "y"),
          non_missing_aes = c("size", "shape"),
          default_aes = aes(
            shape = 19, colour = "black", size = 1.5, fill = NA,
            alpha = NA, stroke = 0.5
          ),

          draw_panel = function(data, panel_scales, coord, na.rm = FALSE) {
            data <- subset(data, select = c(-xend, -yend))
            data <- unique(data)
            coords <- coord$transform(data, panel_scales)
            ggname("geom_nodes",
                   pointsGrob(
                     coords$x, coords$y,
                     pch = coords$shape,
                     gp = gpar(
                       col = alpha(coords$colour, coords$alpha),
                       fill = alpha(coords$fill, coords$alpha),
                       # Stroke is added around the outside of the point
                       fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                       lwd = coords$stroke * .stroke / 2
                     )
                   )
            )
          },

          draw_key = draw_key_point
  )
