
context("UMAP")

skip_if_no_umap_learn <- function() {
  if (!reticulate::py_module_available("umap") &&
      Sys.getenv("BNET_FORCE_UMAP_TESTS") != 1)
    skip("umap-learn not available, install with `pip install umap-learn`")
}

test_that("UMAP python", {
  skip_if_no_umap_learn()

  res1 <- embed(iris[1:4], "UMAP", .mute = c("message", "output"))
  res2 <- embed(iris[1:4], "UMAP", .mute = c("message", "output"), knn = 20)

  expect_s4_class(res1, "dimRedResult")
  expect_equal(res1@method, "UMAP")
  expect_equal(res1@pars$d, "euclidean")
  expect_equal(res1@pars$knn, 15)
  expect_equal(res1@pars$method, "umap-learn")
  expect_equal(res1@pars$ndim, 2)

  expect_s4_class(res2, "dimRedResult")
  expect_equal(res2@method, "UMAP")
  expect_equal(res2@pars$d, "euclidean")
  expect_equal(res2@pars$knn, 20)
  expect_equal(res2@pars$method, "umap-learn")
  expect_equal(res2@pars$ndim, 2)


  expect_true(any(res1@data@data != res2@data@data))

  pred1 <- predict(res1, iris[1:4])
  pred2 <- predict(res2, iris[1:4])

  expect_equal(dim(pred1@data), dim(res1@data@data))
  expect_equal(dim(pred2@data), dim(res2@data@data))
})

test_that("UMAP R", {
  res1 <- embed(iris[1:4], "UMAP", method = "naive", .mute = c("message", "output"))
  res2 <- embed(iris[1:4], "UMAP", method = "naive", .mute = c("message", "output"), knn = 20)

  expect_s4_class(res1, "dimRedResult")
  expect_equal(res1@method, "UMAP")
  expect_equal(res1@pars$d, "euclidean")
  expect_equal(res1@pars$knn, 15)
  expect_equal(res1@pars$method, "naive")
  expect_equal(res1@pars$ndim, 2)

  expect_s4_class(res2, "dimRedResult")
  expect_equal(res2@method, "UMAP")
  expect_equal(res2@pars$d, "euclidean")
  expect_equal(res2@pars$knn, 20)
  expect_equal(res2@pars$method, "naive")
  expect_equal(res2@pars$ndim, 2)

  expect_true(any(res1@data@data != res2@data@data))

  pred1 <- predict(res1, iris[1:4])
  pred2 <- predict(res2, iris[1:4])

  expect_equal(dim(pred1@data), dim(res1@data@data))
  expect_equal(dim(pred2@data), dim(res2@data@data))
})
