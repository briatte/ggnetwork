context("ggnetwork")

library(intergraph)
library(network)
data(emon)
data(flo)

test_that("ggnetwork works", {

  ggnetwork(flo)
  ggnetwork(emon[[1]], layout = "circle")

  expect_error(ggnetwork(1, "could not coerce"))
  expect_error(ggnetwork(flo, layout = "wrong value", "unsupported layout"))

})

test_that("intergraph works", {

  ggnetwork(intergraph::asIgraph(emon[[1]]), layout = "circle")

})

context("utilities")

test_that("load_pkg works", {

  expect_error(load_pkg("wrong value"))

})
