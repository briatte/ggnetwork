#' Blank ggplot2 theme, suited for plotting networks.
#'
#' A \code{ggplot2} theme without lines, borders, axis text or titles, suited
#' for plotting networks.
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param ... other \code{\link{theme}} arguments
#'
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
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param ... other \code{\link{theme}} arguments
#'
#' @export
theme_facet <- function(base_size = 12, base_family = "", ...) {
  theme_blank(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(fill = NA, color = "grey50"),
      ...
    )
}

#' Rescale x to (0, 1), except if x is constant
#'
#' Discussed in PR #32: https://github.com/briatte/ggnetwork/pull/32
#' @param x a vector to rescale
#' @param scale the scale on which to rescale the vector
#'
#' @return The rescaled vector, coerced to a vector if necessary.
#'   If the original vector was constant, all of its values are replaced by 0.5.
#'
#' @author Kipp Johnson
scale_safely <- function(x, scale = diff(range(x))) {
  if (!scale) {
    x <- rep(0.5, length.out = length(x))
  } else {
    x <- scale(x, center = min(x), scale = scale)
  }
  as.vector(x)
}

#' fortify generic
#'
#' See \code{ggplot2::\link[ggplot2]{fortify}} for details.
#'
#' @name fortify
#' @rdname fortify
#' @keywords internal
#' @export
#' @importFrom ggplot2 fortify
NULL

#' unit
#'
#' See \code{ggplot2::\link[ggplot2]{unit}} for details.
#'
#' @name unit
#' @rdname unit
#' @keywords internal
#' @export
#' @importFrom ggplot2 unit
NULL


#' format_fortify
#'
#' a unified function to format \code{\link[network]{network}}
#' or \code{\link[igraph:igraph-package]{igraph}} object.
#'
#' @param model an object of class \code{\link[network]{network}}
#'   or \code{\link[igraph:igraph-package]{igraph}}.
#' @param nodes a nodes object from a call to fortify.
#' @param weights the name of an edge attribute to use as edge weights when
#'   computing the network layout, if the layout supports such weights (see
#'   'Details').
#'   Defaults to \code{NULL} (no edge weights).
#' @param arrow.gap a parameter that will shorten the network edges in order to
#'   avoid overplotting edge arrows and nodes; defaults to \code{0} when the
#'   network is undirected (no edge shortening), or to \code{0.025} when the
#'   network is directed. Small values near \code{0.025} will generally achieve
#'   good results when the size of the nodes is reasonably small.
#' @param by a character vector that matches an edge attribute, which will be
#' @param by a character vector that matches an edge attribute, which will be
#'   used to generate a data frame that can be plotted with
#'   \code{\link[ggplot2]{facet_wrap}} or \code{\link[ggplot2]{facet_grid}}. The
#'   nodes of the network will appear in all facets, at the same coordinates.
#'   Defaults to \code{NULL} (no faceting).
#' @param scale whether to (re)scale the layout coordinates. Defaults to
#'   \code{TRUE}, but should be set to \code{FALSE} if \code{layout} contains
#'   meaningful spatial coordinates, such as latitude and longitude.
#' @param stringsAsFactors whether vertex and edge attributes should be
#'   converted to factors if they are of class \code{character}. Defaults to
#'   the value of \code{getOption("stringsAsFactors")}, which is \code{TRUE} by
#'   default: see \code{\link[base]{data.frame}}.
#' @param .list_vertex_attributes_fun a "list vertex attributes" function.
#' @param .get_vertex_attributes_fun a "get vertex attributes" function.
#' @param .list_edges_attributes_fun a "get edges attributes" function.
#' @param .get_edges_attributes_fun a "get edges attributes" function.
#' @param .as_edges_list_fun a "as edges list" function.
#'
#' @return a \code{\link[base]{data.frame}} object.
#'
#' @keywords internal
#'
format_fortify <- function(
  model,
  nodes = NULL,
  weights = NULL,
  arrow.gap = ifelse(network::is.directed(model), 0.025, 0),
  by = NULL,
  scale = TRUE,
  stringsAsFactors = getOption("stringsAsFactors"),
  .list_vertex_attributes_fun = NULL,
  .get_vertex_attributes_fun = NULL,
  .list_edges_attributes_fun = NULL,
  .get_edges_attributes_fun = NULL,
  .as_edges_list_fun = NULL
) {
  # store coordinates
  nodes <- data.frame(nodes)
  colnames(nodes) <- c("x", "y")

  # rescale coordinates
  if (scale) {
    nodes$x <- scale_safely(nodes$x)
    nodes$y <- scale_safely(nodes$y)
  }

  # import vertex attributes
  if (length(.list_vertex_attributes_fun(model)) > 0) {
    nodes <- cbind.data.frame(
      nodes,
      sapply(
        X = .list_vertex_attributes_fun(model),
        Y = model,
        FUN = function(X, Y) .get_vertex_attributes_fun(Y, X),
        simplify = FALSE
      ),
      stringsAsFactors = stringsAsFactors
    )
  }

  # edge list
  if (inherits(model, "igraph")) {
    edges <- .as_edges_list_fun(model)
  } else {
    edges <- .as_edges_list_fun(model, attrname = weights)
  }

  # edge list (if there are duplicated rows)
  if (nrow(edges[, 1:2, drop = FALSE]) > nrow(unique(edges[, 1:2, drop = FALSE]))) {
    warning("duplicated edges detected")
  }

  edges <- data.frame(nodes[edges[, 1], c("x", "y")], nodes[edges[, 2], c("x", "y")])
  colnames(edges) <- c("x", "y", "xend", "yend")

  # arrow gap (thanks to @heike and @ethen8181 for their work on this issue)
  if (arrow.gap > 0) {
    x.length <- edges$xend - edges$x
    y.length <- edges$yend - edges$y
    arrow.gap <- arrow.gap / sqrt(x.length^2 + y.length^2)
    edges$xend <- edges$x + (1 - arrow.gap) * x.length
    edges$yend <- edges$y + (1 - arrow.gap) * y.length
  }

  # import edge attributes
  if (length(.list_edges_attributes_fun(model)) > 0) {
    edges <- cbind.data.frame(
      edges,
      sapply(
        X = .list_edges_attributes_fun(model),
        Y = model,
        FUN = function(X, Y) .get_edges_attributes_fun(Y, X),
        simplify = FALSE
      ),
      stringsAsFactors = stringsAsFactors
    )
  }

  if (nrow(edges) > 0) {
    # drop "na" columns created by 'network' methods
    # this is to ensure consistency with 'igraph' methods
    if ("na" %in% colnames(nodes)) nodes$na <- NULL
    if ("na" %in% colnames(edges)) edges$na <- NULL

    # merge edges and nodes data
    edges <- merge(nodes, edges, by = c("x", "y"), all = TRUE)

    # add missing columns to nodes data
    nodes$xend <- nodes$x
    nodes$yend <- nodes$y
    # names(nodes) <- names(edges)[1:ncol(nodes)] # columns are already named from 'nodes' and 'edges'

    # make nodes data of identical dimensions to edges data
    nodes[, setdiff(names(edges), names(nodes))] <- NA

    # panelize nodes (for temporal networks)
    if (!is.null(by)) {
      nodes <- lapply(sort(unique(edges[, by])), function(x) {
        y <- nodes
        y[, by] <- x
        y
      })
      nodes <- do.call(rbind, nodes)
    }

    # return a data frame with network.size(model) + network.edgecount(model) rows,
    # or length(unique(edges[, by ])) * network.size(model) + network.edgecount(model)
    # rows if the nodes have been panelized

    # [NOTE] `edges` has to be passed first to `rbind` in order for the edge
    # attributes (e.g. factor) to be preserved in the result; this should not
    # affect plotting the result, but differs from `ggnetwork` 0.5.1: `nodes`
    # is now at the end of the result rather at the beginning

    return(unique(rbind(edges[!is.na(edges$xend), ], nodes)))
  } else {
    # add missing columns to nodes data
    nodes$xend <- nodes$x
    nodes$yend <- nodes$y
    return(nodes)
  }
}
