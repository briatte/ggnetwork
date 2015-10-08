#' @keywords internal
load_pkg <- function(x) {
  if (!require(x, character.only = TRUE)) {
    stop("install the '", x, "' package to use ggnetwork")
  }
}

#' Blank theme
#'
#' A \code{ggplot2} theme without lines, borders, axis text or titles, suited
#' for plotting networks.
theme_blank <- function(base_size = 12, base_family = "") {
  theme(line = element_blank(),
        rect = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        complete = TRUE)
}
