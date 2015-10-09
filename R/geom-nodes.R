#' Draw the nodes of a network.
#'
#' All arguments to this geom are identical to those of
#' \code{\link[ggplot2]{geom_point}}.
#' @param mapping see \code{\link[ggplot2]{geom_point}}
#' @param data see \code{\link[ggplot2]{geom_point}}
#' @param position see \code{\link[ggplot2]{geom_point}}
#' @param na.rm see \code{\link[ggplot2]{geom_point}}
#' @param show.legend see \code{\link[ggplot2]{geom_point}}
#' @param inherit.aes see \code{\link[ggplot2]{geom_point}}
#' @param ... see \code{\link[ggplot2]{geom_point}}
#' @importFrom ggplot2 layer
#' @export
geom_nodes <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE, ...) {

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

#' Label the nodes of a network.
#'
#' All arguments to this geom are identical to those of
#' \code{\link[ggplot2]{geom_text}}.
#' @param mapping see \code{\link[ggplot2]{geom_text}}
#' @param data see \code{\link[ggplot2]{geom_text}}
#' @param position see \code{\link[ggplot2]{geom_text}}
#' @param parse see \code{\link[ggplot2]{geom_text}}
#' @param show.legend see \code{\link[ggplot2]{geom_text}}
#' @param inherit.aes see \code{\link[ggplot2]{geom_text}}
#' @param ... see \code{\link[ggplot2]{geom_text}}
#' @param nudge_x see \code{\link[ggplot2]{geom_text}}
#' @param nudge_y see \code{\link[ggplot2]{geom_text}}
#' @param check_overlap see \code{\link[ggplot2]{geom_text}}
#' @importFrom ggplot2 layer position_nudge
#' @export
geom_nodetext <- function(mapping = NULL, data = NULL,
                          position = "identity", parse = FALSE,
                          show.legend = NA, inherit.aes = TRUE,
                          ..., nudge_x = 0, nudge_y = 0,
                          check_overlap = FALSE) {

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
      parse = parse,
      check_overlap = check_overlap,
      ...
    )
  )

}

#' @importFrom ggplot2 ggproto
#' @keywords internal
StatNodes <-
  ggplot2::ggproto("StatNodes", Stat,
                   compute_layer = function(data, scales, params) {
                     unique(subset(data, select = c(-xend, -yend)))
                   }
  )
