#' Draw the edges of a network.
#'
#' All arguments to this function are identical to those of
#' \code{\link[ggplot2]{geom_segment}}, including \code{arrow}.
#' @param mapping see \code{\link[ggplot2]{geom_segment}}
#' @param data see \code{\link[ggplot2]{geom_segment}}
#' @param stat see \code{\link[ggplot2]{geom_segment}}
#' @param position see \code{\link[ggplot2]{geom_segment}}
#' @param arrow see \code{\link[ggplot2]{geom_segment}}
#' @param na.rm see \code{\link[ggplot2]{geom_segment}}
#' @param show.legend see \code{\link[ggplot2]{geom_segment}}
#' @param inherit.aes see \code{\link[ggplot2]{geom_segment}}
#' @param ... see \code{\link[ggplot2]{geom_segment}}
#' @importFrom ggplot2 layer ggproto
#' @export
geom_edges <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", arrow = NULL,
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                       ...) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomEdges,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )

}

#' @rdname geom_edges
#' @format NULL
#' @usage NULL
#' @export
GeomEdges <-
  ggplot2::ggproto("GeomEdges", Geom,
                   required_aes = c("x", "y", "xend", "yend"),
                   non_missing_aes = c("linetype", "size", "shape"),
                   default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

                   draw_panel = function(data, panel_scales, coord, arrow = NULL, na.rm = FALSE) {

                     data <- remove_missing(data, na.rm = na.rm,
                                            c("i", "j", "linetype", "size", "shape"),
                                            name = "geom_edges")

                     if (empty(data)) return(zeroGrob())

                     if (coord$is_linear()) {

                       coord <- coord$transform(data, panel_scales)
                       return(
                         ggname("geom_edges",
                                segmentsGrob(coord$x, coord$y, coord$xend, coord$yend,
                                             default.units = "native",
                                             gp = gpar(
                                               col = alpha(coord$colour, coord$alpha),
                                               fill = alpha(coord$colour, coord$alpha),
                                               lwd = coord$size * .pt,
                                               lty = coord$linetype
                                             ),
                                             arrow = arrow)
                         ))
                     }

                     data$group <- 1:nrow(data)
                     starts <- subset(data, select = c(-xend, -yend))
                     ends <- plyr::rename(subset(data, select = c(-x, -y)),
                                          c("xend" = "x", "yend" = "y"),
                                          warn_missing = FALSE)

                     pieces <- rbind(starts, ends)
                     pieces <- pieces[order(pieces$group),]

                     GeomPath$draw_panel(pieces, panel_scales, coord, arrow = arrow,
                                         lineend = lineend)
                   },

                   draw_key = draw_key_path
  )
