#' Fortify method for networks of class \code{\link[igraph:igraph-package]{igraph}}
#'
#' This function requires the \code{\link[intergraph:intergraph-package]{intergraph}}
#' package to be installed.
#' @param model an object of class \code{\link[igraph:igraph-package]{igraph}},
#' which will be converted to an object of class \code{\link[network]{network}}
#' with the \code{\link[intergraph]{asNetwork}} function before being passed to
#' the \code{\link{fortify.network}} function.
#' @param ... additional parameters for the \code{fortify.network} function; see
#' \code{\link{fortify.network}}.  If \code{.convert.igraph} is FALSE, these 
#' will be passed to 
#' @param .layout character string corresponding to an igraph layout function
#' (for example, .layout = "with_fr" will call layout_with_fr).
#' @param .convert.igraph logical value indicating whether an igraph value
#' should be converted to a statnet network object.  If TRUE, the network will
#' be converted using \code{\link[intergraph]{asNetwork}}; if FALSE, the layout
#' will be calculated using the \code{\link[igraph:igraph-package]{igraph}}
#' layout functions.
#' @method fortify igraph
#' @importFrom utils installed.packages
#' @export
fortify.igraph <- function(model, ..., .layout = "nicely", .convert.igraph = T) {

  # CRAN behavior included by default
  if (.convert.igraph) {
    if ("intergraph" %in% rownames(utils::installed.packages())) {
  
      fortify.network(intergraph::asNetwork(model), ...)
  
    } else {
  
      stop("install the 'intergraph' package to use igraph objects with ggnetwork")
  
    }
    
  } else {  # use igraph functions to mimic fortify.network
    
    x = model
    
    # node placement
    if (class(.layout) == "matrix" &&
        nrow(.layout) == igraph::gorder(x) &&
        ncol(.layout) == 2) {
      nodes = .layout[, 1:2 ]
    } else {
      
      # Use the default igraph layout if no layout is specified
      if (is.null(.layout)) {
        .layout <- "nicely"
      } 
      
      # Call igraph layout function
      print(.layout)
      current.layout = paste0("layout_", .layout)
      ns <- loadNamespace("igraph")
      if (!exists(.layout, envir=ns, inherits=FALSE)) {
        stop("unsupported layout")
      }
      nodes = do.call( utils::getFromNamespace(current.layout, ns),
                       list(x, ...))
    }
    
    # store coordinates
    nodes = data.frame(nodes)
    names(nodes) = c("x", "y")
    
    # rescale coordinates
    nodes$x = scale(nodes$x, center = min(nodes$x), scale = diff(range(nodes$x)))
    nodes$y = scale(nodes$y, center = min(nodes$y), scale = diff(range(nodes$y)))
    
    # import vertex attributes
    for (y in igraph::list.vertex.attributes(x)) {
      nodes = cbind(nodes, igraph::get.vertex.attribute(x, y))
      names(nodes)[ncol(nodes)] = y
    }
    
    # edge list
    if (is.null(weights)) {
      edges <- igraph::as_edgelist(x, names = F)
    } else {
      # See https://stackoverflow.com/questions/16289353/r-igraph-display-edge-weights-in-an-edge-list
      edges <- cbind(igraph::as_edgelist(x, names = F), 
                     igraph::edge_attr(weights))
    }
    
    # edge list (if there are duplicated rows)
    if (nrow(edges[, 1:2]) > nrow(unique(edges[, 1:2]))) {
      warning("duplicated edges detected")
    }
    
    edges = data.frame(nodes[edges[, 1], 1:2], nodes[edges[, 2], 1:2])
    names(edges) = c("x", "y", "xend", "yend")
    
    # arrow gap (thanks to @heike and @ethen8181 for their work on this issue)
    if (arrow.gap > 0) {
      x.length = with(edges, xend - x)
      y.length = with(edges, yend - y)
      arrow.gap = with(edges, arrow.gap / sqrt(x.length ^ 2 + y.length ^ 2))
      edges = transform(
        edges,
        # x = x + arrow.gap * x.length,
        # y = y + arrow.gap * y.length,
        xend = x + (1 - arrow.gap) * x.length,
        yend = y + (1 - arrow.gap) * y.length
      )
    }
    
    # import edge attributes
    for (y in igraph::list.edge.attributes(x)) {
      edges = cbind(edges, igraph::get.edge.attribute(x, y))
      names(edges)[ncol(edges)] = y
    }
    
    # merge edges and nodes data
    edges = merge(nodes, edges, by = c("x", "y"), all = TRUE)
    
    # add missing columns to nodes data
    nodes$xend = nodes$x
    nodes$yend = nodes$y
    names(nodes) = names(edges)[1:ncol(nodes)]
    
    # make nodes data of identical dimensions to edges data
    for (y in names(edges)[(1 + ncol(nodes)):ncol(edges)]) {
      nodes = cbind(nodes, NA)
      names(nodes)[ncol(nodes)] = y
    }
    
    # panelize nodes (for temporal networks)
    if (!is.null(by)) {
      nodes = lapply(sort(unique(edges[, by ])), function(x) {
        y = nodes
        y[, by ] = x
        y
      })
      nodes = do.call(rbind, nodes)
    }
    
    # return a data frame with network.size(x) + network.edgecount(x) rows,
    # or length(unique(edges[, by ])) * network.size(x) + network.edgecount(x)
    # rows if the nodes have been panelized
    unique(rbind(nodes, edges[ !is.na(edges$xend), ]))
    
    
    
  }

}
