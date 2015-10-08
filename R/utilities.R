#' @author Hadley Wickham
#' @source ggplot2/R/utilities.r
#' @keywords internal
empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0
}

#' @author Hadley Wickham
#' @source ggplot2/R/utilities.r
#' @keywords internal
try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    library(package, character.only = TRUE)
    return(invisible())
  }

  stop("Package `", package, "` required for `", fun , "`.\n",
       "Please install and try again.", call. = FALSE)
}

#' @author Hadley Wickham
#' @source ggplot2/R/utilities-grid.r
#' @importFrom grid grobName
#' @keywords internal
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}
