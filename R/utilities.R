#' @keywords internal
load_pkg <- function(x) {
  if (!require(x, character.only = TRUE)) {
    stop("install the '", x, "' package to use ggnetwork")
  }
}
