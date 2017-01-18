
context("the dimRedData class")

test_that("constructor", {
    expect_equal(dimRedData(), new("dimRedData",
                                   data = matrix(numeric(0), nrow = 0, ncol = 0),
                                   meta = data.frame()))
    expect_error(dimRedData(iris))
    expect_s4_class(dimRedData(iris[, 1:4], iris[, 5]), "dimRedData")
    expect_s4_class(dimRedData(iris[, 1:4]), "dimRedData")
    expect_error(dimRedData(iris))
})

test_that("conversion functions", {
    expect_equal(as(iris[, 1:4], "dimRedData"), dimRedData(iris[, 1:4]))
    expect_error(as(iris, "dimRedData"))
    expect_equal(as(loadDataSet("Iris"), "data.frame"),
                 as.data.frame(loadDataSet("Iris")))
    expect_equivalent(as.dimRedData(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, iris),
                      loadDataSet("Iris"))
})

test_that("misc functions", {
    Iris <- loadDataSet("Iris")
    expect_equal(getData(Iris), Iris@data)
    expect_equal(getMeta(Iris), Iris@meta)
    expect_equal(nrow(Iris), 150)
    expect_equal(Iris[1:4], Iris[1:4, ])
    expect_equal(Iris[1:4], Iris[c(rep(TRUE, 4), rep(FALSE, 146))])
    expect_equal(Iris[1:4], Iris[c(rep(TRUE, 4), rep(FALSE, 146)), ])
})
