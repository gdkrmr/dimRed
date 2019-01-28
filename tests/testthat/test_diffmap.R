context("DiffusionMaps")

test_that("DiffusionMaps", {
  expect_s4_class(embed(iris[1:4], "DiffusionMaps", ndim = 1,
                        .mute = c("message", "output")),
                  "dimRedResult")
  x <- embed(iris[1:4], "DiffusionMaps", ndim = 1,
             .mute = c("message", "output"))
  expect_equal(dim(x@data@data), c(150, 1))
})
