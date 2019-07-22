test_that("ggnetwork works", {
  data(emon, package = "network")
  expect_error(ggnetwork(-999, "could not coerce"))
  expect_error(ggnetwork(emon[[1]], layout = -999, "unsupported layout"))

  # facet by edge attribute
  expect_s3_class({
    ggnetwork(emon[[1]], arrow.gap = 0.02, by = "Frequency")
  }, class = "data.frame")

  # user-provided layout
  expect_s3_class({
    ggnetwork(emon[[1]], layout = matrix(runif(28), ncol = 2))
  }, class = "data.frame")

  # edge weights in layout
  expect_s3_class({
    ggnetwork(emon[[1]], layout = "kamadakawai", weights = "Frequency")
  }, class = "data.frame")

  # duplicated edges warning
  expect_warning(
    ggnetwork(rbind(
      matrix(c(1:2, 2:1), nrow = 2),
      matrix(c(1:2, 2:1), nrow = 2)
    )),
    "duplicated edges"
  )
})
