#' Blank ggplot2 theme, suited for plotting networks.
#'
#' A \code{ggplot2} theme without lines, borders, axis text or titles, suited
#' for plotting networks.
#' @param base_size base font size
#' @param base_family base font family
#' @param ... other \code{\link{theme}} arguments
#' @export
theme_blank <- function(base_size = 12, base_family = "", ...) {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      ...
    )
}

#' Blank ggplot2 theme with a panel border.
#'
#' A variation of \code{\link{theme_blank}} that adds a panel border to the
#' plot, which is often suitable for plotting faceted networks.
#' @param base_size base font size
#' @param base_family base font family
#' @param ... other \code{\link{theme}} arguments
#' @export
theme_facet <- function(base_size = 12, base_family = "", ...) {
  theme_blank(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(fill = NA, color = "grey50"),
      ...
    )
}
