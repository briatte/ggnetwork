context("Test ggnetwork")

data(emon, package = "network")

test_that("ggnetwork works", {
  expect_error(ggnetwork(-999, "could not coerce"))
  expect_error(ggnetwork(emon[[1]], layout = -999, "unsupported layout"))

  # facet by edge attribute
  ggnetwork(emon[[1]], arrow.gap = 0.02, by = "Frequency")

  # user-provided layout
  ggnetwork(emon[[1]], layout = matrix(runif(28), ncol = 2))

  # edge weights in layout
  ggnetwork(emon[[1]], layout = "kamadakawai", weights = "Frequency")

  # duplicated edges warning
  expect_warning(
    ggnetwork(rbind(
      matrix(c(1:2, 2:1), nrow = 2),
      matrix(c(1:2, 2:1), nrow = 2)
    )),
    "duplicated edges"
  )
})
