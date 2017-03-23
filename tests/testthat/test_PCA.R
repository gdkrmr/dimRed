
data(iris)
context("PCA")

test_that("general data conversions", {
    irisData <- as(iris[, 1:4], "dimRedData")
    expect_equal(class(irisData)[1], "dimRedData")

    irisParsCS <- list(center = TRUE,  scale. = TRUE)
    irisParsC  <- list(center = TRUE,  scale. = FALSE)
    irisParsS  <- list(center = FALSE, scale. = TRUE)
    irisPars   <- list(center = FALSE, scale. = FALSE)

    irisResCS <- do.call(function(...) embed(irisData, "PCA", ...), irisParsCS)
    irisResS  <- do.call(function(...) embed(irisData, "PCA", ...), irisParsS)
    irisResC  <- do.call(function(...) embed(irisData, "PCA", ...), irisParsC)
    irisRes   <- do.call(function(...) embed(irisData, "PCA", ...), irisPars)
    expect_equal(class(irisResCS)[1], "dimRedResult")
    expect_equal(class(irisResS)[1],  "dimRedResult")
    expect_equal(class(irisResC)[1],  "dimRedResult")
    expect_equal(class(irisRes)[1],   "dimRedResult")

    expect_equal(irisResCS@apply(irisData), irisResCS@data)
    expect_equal(irisResS@apply(irisData),  irisResS@data)
    expect_equal(irisResC@apply(irisData),  irisResC@data)
    expect_equal(irisRes@apply(irisData),   irisRes@data)

    expect(sqrt(mean(
            (irisResCS@inverse(irisResCS@data)@data - irisData@data) ^ 2
        )) < 0.3,
        "error too large"
    )
    expect(sqrt(mean(
            (irisResS@inverse(irisResS@data)@data   - irisData@data) ^ 2
        )) < 0.3,
        "error too large"
    )
    expect(sqrt(mean(
            (irisResC@inverse(irisResC@data)@data   - irisData@data) ^ 2
        )) < 0.3,
        "error too large"
    )
    expect(sqrt(mean(
            (irisRes@inverse(irisRes@data)@data     - irisData@data) ^ 2
        )) < 0.3,
        "error too large"
    )

    scale2 <- function(x, center, scale.) scale(x, center, scale.)
    expect_equal(
      do.call(function(...) scale2(iris[1:4], ...) %*% getRotationMatrix(irisResCS), irisParsCS),
      getData( getDimRedData(irisResCS) )
    )
    expect_equal(
      do.call(function(...) scale2(iris[1:4], ...) %*% getRotationMatrix(irisResS), irisParsS),
      getData( getDimRedData(irisResS) )
    )
    expect_equal(
      do.call(function(...) scale2(iris[1:4], ...) %*% getRotationMatrix(irisResC), irisParsC),
      getData( getDimRedData(irisResC) )
    )
    expect_equal(
      do.call(function(...) scale2(iris[1:4], ...) %*% getRotationMatrix(irisRes), irisPars),
      getData( getDimRedData(irisRes) )
    )
})
