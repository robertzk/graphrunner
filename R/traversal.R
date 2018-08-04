#' An R6 representation of a graph traversal strategy.
#'
#' A graph traversal strategy specifies how to execute tasks on
#' a given graph.
#'
#' @name graphTraversalStrategy
#' @format NULL
#' @docType class
graphTraversalStrategy <- R6::R6Class('graphTraversalStrategy',
  public = list(
    .visited = logical(0),
    .strategy = identity,

    initialize = function(strategy) {
      stopifnot(is.function(strategy))
      self$.strategy <- strategy
    },

    traverse = function(node) {
      .NotYetImplemented()
    }
  )
)

#' A breadth-first traversal strategy.
#'
#' This graph traversal strategy begins from the boot node and
#' traverses "breadth-first" outward.
#'
#' @name graphTraversalStrategy
#' @format NULL
#' @docType class
graphBFSTraversalStrategy <- R6::R6Class('graphBFSTraversalStrategy',
  inherit = graphTraversalStrategy,
  public = list(
    traverse = function(node, directed = FALSE) {
      if (is.graph(node)) {
        self$.visited <- logical(0)
        on.exit(self$.visited <- logical(0))
        self$traverse(graph$bootnode())
      } else if (is.graphNode(node)) {
        if (self$.visited[node$address()]) {
          return()
        }
        self$.strategy(node)
        self$.visited[node$address()] <- TRUE
        edges <- graph$bootnode$edges()
        if (!isTRUE(directed)) {
          # Go through edges *and* backwards edges in case some nodes
          # are directionally isolated (e.g., they flow out to the bootnode,
          # but the bootnode does not flow back to the through any path).
          edges <- c(edges, graph$bootnode$backwards_edges())
        }
        for (edge in edges) {
          if (!self$.visited[edge$address()]) {
            self$traverse(edge)
          }
        }
      } else {
        stop("Can only traverse a graph or graphNode.")
      }
    }
  )
)

