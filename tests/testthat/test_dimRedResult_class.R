test_that("predict/inverse methods", {
  dat <- loadDataSet("Iris")
  emb <- embed(dat, "PCA", ndim = 4)
  pred <- predict(emb, dat)
  inv <- inverse(emb, pred)
  expect_equal(getDimRedData(emb), pred)
  expect_equal(dat, inv)

  emb2 <- embed(dat, "tSNE")
  expect_error(predict(emb2, dat))
  expect_error(inverse(emb2, dat))
})

test_that("conversion", {
  iris_data_frame_as <- as(embed(loadDataSet("Iris"), "PCA"), "data.frame")
  expect_equal(colnames(iris_data_frame_as), c("meta.Species", "PC1", "PC2", colnames(iris)[-5]))
})
