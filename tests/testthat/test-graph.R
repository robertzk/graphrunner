describe("graphNode", {
  context("graphNode")
  
  test_that("it can initialize correctly", {
    node <- graphNode$new(1)
    expect_equal(node$value(), 1)
  })
})

describe("graph", {
  context("graph")
  
  test_that("it can initialize correctly", {
    expect_true(TRUE)
  })
})

