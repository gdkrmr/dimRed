test_that("standard method is PCA", {

  res <- embed(iris[1:4])
  expect_equal(res@method, "PCA")

})

test_that("correctly convert .keep.org.data argument", {
  res1 <- embed(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                iris, "PCA", .keep.org.data = FALSE)


  res2 <- embed(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                iris, "PCA", .keep.org.data = TRUE)

  attr(res2@org.data, "assign") <- NULL

  expect_equal(
    unname(as.matrix(iris[1:4])),
    unname(as.matrix(getOrgData(res2)@data))
  )

})
