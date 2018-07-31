
context("embed")

test_that("standard method is PCA", {

  res <- embed(iris[1:4])
  expect_equal(res@method, "PCA")

})
