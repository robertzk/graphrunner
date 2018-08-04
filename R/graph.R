## We use [R6](https://github.com/wch/R6) instead of the built-in 
## [reference classes](https://stat.ethz.ch/R-manual/R-devel/library/methods/html/refClass.html) 
## for several reasons.
##
##    1. Their definition is much more compact.
##    2. It is possible to extend R6 definitions cross-packages.
##    3. They suppor the notion of public and private membership.
##
## A graph is better represented as a reference object, rather than an
## S3 or S4 class, to allow for mutability. That being said, it is certainly
## possible to create an S3 or S4 equivalent.
##
## A graph node is primarily defined by its **value** and its **edges**.
## A (connected) graph is identified by any node, as it is possible to
## recover the rest of the graph through traversal.
##
#' An R6 representation of a node for a graph data structure.
#'
#' @name graphNode
#' @format NULL
#' @docType class
graphNode_ <- R6::R6Class("graphNode",
  public = list(
    .edges = list(),                
    .value = NULL,

    initialize = function(value) {
      self$.value <- value
    },

    value = function() {
      self$.value
    },

    add_edge = function(node) {
      stopifnot(is(node, "graphNode"))
      # Avoid copy creation.
      address <- eval.parent(substitute(pryr::address(node)))
      self$.edges[[address]] <- node
    },

    edges = function() {
      self$.edges
    },

    num_edges = function() {
      length(self$.edges)
    }
  )
)

## A little trick to ensure that a graphNode can be constructed both as
## `graphNode(...) and graphNode$new(...)`.
#' @rdname graphNode
#' @param ... Arguments to pass to graphNode initialization.
#' @export
graphNode <- structure(
  function(...) { graphNode_$new(...) },
  class = "graphNode_"
)

## To make the above trick work, we need to prevent access to everything except
## `new`.
#' @export
`$.graphNode_` <- function(...) {
  stopifnot(identical(..2, "new"))
  ..1
}

#' Check whether an R object is a graphNode object
#'
#' @export
#' @param obj any object.
#' @return \code{TRUE} if the object is of class
#'    \code{graphNode}, \code{FALSE} otherwise.
is.graphNode <- function(obj) {
  inherits(obj, "graphNode")
}

# # # # Graph # # # #

#' An R6 representation of a graph data structure.
#'
#' All graphs are assumed to be connected (if they are not, use a list
#' of graphs instead). Any distinguished canonical node on the graph,
#' called the boot node, will be used to represent the entire graph
#' and will be the first node for purposes of traversal.
#'
#' @name graph
#' @format NULL
#' @docType class
graph_ <- R6::R6Class("graph",
  public = list(
    .bootnode = NULL,

    initialize = function(bootnode) {
      stopifnot(is(bootnode, "graphNode"))
      self$.bootnode <- bootnode
    },

    bootnode_value = function() {
      self$.bootnode$value()
    }
  )
)

## A little trick to ensure that a graph can be constructed both as
## `graph(...) and graph$new(...)`.
#' @rdname graph
#' @param ... Arguments to pass to graph initialization.
#' @export
graph <- structure(
  function(...) { graph_$new(...) },
  class = "graph_"
)

## To make the above trick work, we need to prevent access to everything except
## `new`.
#' @export
`$.graph_` <- function(...) {
  stopifnot(identical(..2, "new"))
  ..1
}

#' Check whether an R object is a graph object
#'
#' @export
#' @param obj any object.
#' @return \code{TRUE} if the object is of class
#'    \code{graph}, \code{FALSE} otherwise.
is.graph <- function(obj) {
  inherits(obj, "graph")
}

