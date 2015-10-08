#' Label the edges of a network.
#'
#' All arguments to this function are identical to those of
#' \code{\link[ggplot2]{geom_text}}. The \code{fill} aesthetic can be used to
#' set the background color of the labels, which defauts to \code{"white"}.
#' @param mapping see \code{\link[ggplot2]{geom_text}}
#' @param data see \code{\link[ggplot2]{geom_text}}
#' @param stat see \code{\link[ggplot2]{geom_text}}
#' @param position see \code{\link[ggplot2]{geom_text}}
#' @param parse see \code{\link[ggplot2]{geom_text}}
#' @param show.legend see \code{\link[ggplot2]{geom_text}}
#' @param inherit.aes see \code{\link[ggplot2]{geom_text}}
#' @param ... see \code{\link[ggplot2]{geom_text}}
#' @param nudge_x see \code{\link[ggplot2]{geom_text}}
#' @param nudge_y see \code{\link[ggplot2]{geom_text}}
#' @param check_overlap see \code{\link[ggplot2]{geom_text}}
#' @importFrom ggplot2 layer ggproto position_nudge
#' @export
geom_edgetext <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", parse = FALSE,
                          show.legend = NA, inherit.aes = TRUE,
                          ..., nudge_x = 0, nudge_y = 0,
                          check_overlap = FALSE) {

  if (!missing(nudge_x) || !missing(nudge_y)) {

    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)

  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomEdgeText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      ...
    )
  )

}

#' @rdname geom_edgetext
#' @format NULL
#' @usage NULL
#' @export
GeomEdgeText <-
  ggproto("GeomEdgeText", Geom,
          required_aes = c("x", "y", "label"),

          default_aes = aes(
            colour = "black", fill = "white", size = 3.88, angle = 0, hjust = 0.5,
            vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
          ),

          draw_panel = function(data, panel_scales, coord, parse = FALSE,
                                na.rm = FALSE, check_overlap = FALSE) {

            data$x = (data$x + data$xend) / 2
            data$y = (data$y + data$yend) / 2
            data <- subset(data, select = c(-xend, -yend))
            data <- unique(data)

            lab <- data$label
            if (parse) {
              lab <- parse(text = lab)
            }

            data <- coord$transform(data, panel_scales)
            if (is.character(data$vjust)) {
              data$vjust <- compute_just(data$vjust, data$y)
            }
            if (is.character(data$hjust)) {
              data$hjust <- compute_just(data$hjust, data$x)
            }

            grobTree(
              pointsGrob(
                data$x, data$y,
                pch = 19, #coords$shape,
                gp = gpar(
                  col = data$fill, # alpha(coords$colour, coords$alpha),
                  fill = data$fill, # alpha(coords$fill, coords$alpha),
                  # Stroke is added around the outside of the point
                  fontsize = 1.5 * data$size * .pt, # coords$size * .pt + coords$stroke * .stroke / 2,
                  lwd = 0 # coords$stroke * .stroke / 2
                )
              ),
              textGrob(
                lab,
                data$x, data$y, default.units = "native",
                hjust = data$hjust, vjust = data$vjust,
                rot = data$angle,
                gp = gpar(
                  col = alpha(data$colour, data$alpha),
                  fontsize = data$size * .pt,
                  fontfamily = data$family,
                  fontface = data$fontface,
                  lineheight = data$lineheight
                ),
                check.overlap = check_overlap
              )
            )
          },

          draw_key = draw_key_text
  )
