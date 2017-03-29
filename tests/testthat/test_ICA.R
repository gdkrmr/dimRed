
data(iris)
context("FastICA")

test_that("general data conversions", {
    irisData <- as(iris[, 1:4], "dimRedData")
    expect_equal(class(irisData)[1], "dimRedData")

    irisRes <- embed(irisData, "FastICA")
    expect_equal(class(irisRes)[1],   "dimRedResult")

    expect_equal(irisRes@apply(irisData), irisRes@data)

    expect(sqrt(mean(
            (irisRes@inverse(irisRes@data)@data - irisData@data) ^ 2
        )) < 0.3,
        "error too large"
    )

    expect_equal(
      scale(iris[1:4], TRUE, FALSE) %*% getRotationMatrix(irisRes),
      unname(as.matrix(getData( getDimRedData(irisRes) )) )
    )
})
