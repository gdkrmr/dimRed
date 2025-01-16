test_that("standard method is PCA", {

  res <- embed(iris[1:4])
  expect_equal(res@method, "PCA")

})

test_that("correctly convert .keep.org.data argument", {
  res1 <- embed(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
        iris, "PCA", .keep.org.data = FALSE)


  res2 <- embed(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
        iris, "PCA", .keep.org.data = TRUE)

  expect_equal(as.matrix(iris[1:4]), getOrgData(res2))
})
