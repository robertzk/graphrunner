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
    .strategy = identity
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
  )
)

