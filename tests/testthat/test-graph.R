describe("graphNode", {
  context("graphNode")
  
  test_that("it can initialize correctly", {
    node <- graphNode$new(1)
    expect_equal(node$value(), 1)
  })

  test_that("it can add an edge", {
    node  <- graphNode$new(1)
    node2 <- graphNode$new(2)
    node3 <- graphNode$new(3)
    node$add_edge(node2)
    node$add_edge(node3)
    expect_equal(node$num_edges(), 2)
  })

  test_that("adding a duplicate edge does not increase size", {
    node  <- graphNode$new(1)
    node2 <- graphNode$new(2)
    node$add_edge(node2)
    expect_equal(node$num_edges(), 1)
    node$add_edge(node2)
    expect_equal(node$num_edges(), 1)
    node3 <- graphNode$new(3)
    node$add_edge(node3)
    expect_equal(node$num_edges(), 2)
  })

  test_that("it errors with invalid edges", {
    node  <- graphNode$new(1)
    lapply(list(1, NULL, emptyenv(), "foo", identity), function(obj) {
      expect_error(node$add_edge(obj))
    })
  })
})

describe("graph", {
  context("graph")

  test_that("it errors with invalid inputs", {
    node  <- graphNode$new(1)
    lapply(list(1, NULL, emptyenv(), "foo", identity), function(obj) {
      expect_error(graph$new(obj))
    })
  })
  
  test_that("it can initialize correctly", {
    node  <- graphNode$new(1)
    graph <- graph$new(node)
    expect_equal(graph$bootnode_value(), 1)
  })
})

