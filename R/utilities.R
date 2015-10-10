#' @keywords internal
load_pkg <- function(x) {
  if (!require(x, character.only = TRUE)) {
    stop("install the '", x, "' package to use ggnetwork")
  }
}

#' Blank ggplot2 theme, suited for plotting networks.
#'
#' A \code{ggplot2} theme without lines, borders, axis text or titles, suited
#' for plotting networks.
#' @param base_size base font size
#' @param base_family base font family
#' @importFrom grid unit
#' @export
theme_blank <- function(base_size = 12, base_family = "") {
  ggplot2::theme(line = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        plot.margin = grid::unit(c(0, 0, 0, 0), "lines"),
        complete = TRUE)
}
