
context("PCA L1")

test_that("general data conversions", {
    skip_if_not_installed("pcaL1")
    irisData <- as(iris[, 1:4], "dimRedData")
    expect_equal(class(irisData)[1], "dimRedData")

    irisParsCS <- list(center = TRUE,  .mute = c("message", "output"), ndim = 4, scale. = TRUE,  projections = "l1", fun = "l1pca")
    irisParsC  <- list(center = TRUE,  .mute = c("message", "output"), ndim = 4, scale. = FALSE, projections = "l1", fun = "l1pca")
    irisParsS  <- list(center = TRUE,  .mute = c("message", "output"), ndim = 4, scale. = TRUE,  projections = "l1", fun = "l1pcahp")
    irisPars   <- list(center = FALSE, .mute = c("message", "output"), ndim = 4, scale. = FALSE, projections = "l1", fun = "l1pcastar")

    irisResCS <- do.call(function(...) embed(irisData, "PCA_L1", ...), irisParsCS)
    irisResS  <- do.call(function(...) embed(irisData, "PCA_L1", ...), irisParsS)
    irisResC  <- do.call(function(...) embed(irisData, "PCA_L1", ...), irisParsC)
    irisRes   <- do.call(function(...) embed(irisData, "PCA_L1", ...), irisPars)

    expect_equal(4, getNDim(irisResCS))
    expect_equal(4, getNDim(irisResS))
    expect_equal(4, getNDim(irisResC))
    expect_equal(4, getNDim(irisRes))

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
      do.call(function(...) scale2(iris[1:4], ...) %*% getRotationMatrix(irisResCS),
              irisParsCS[c("center", "scale.")]),
      getData( getDimRedData(irisResCS) ),
      tolerance = 1e-2
    )

    expect_equal(
      do.call(function(...) scale2(iris[1:4], ...) %*% getRotationMatrix(irisResS),
              irisParsS[c("center", "scale.")]),
      getData( getDimRedData(irisResS) ),
      tolerance = 1e-2
    )

    expect_equal(
      do.call(function(...) scale2(iris[1:4], ...) %*% getRotationMatrix(irisResC),
              irisParsC[c("center", "scale.")]),
      getData( getDimRedData(irisResC) ),
      tolerance = 1e-2
    )
    expect_equal(
      do.call(function(...) scale2(iris[1:4], ...) %*% getRotationMatrix(irisRes),
              irisPars[c("center", "scale.")]),
      getData( getDimRedData(irisRes) ),
      tolerance = 1e-2
    )

    expect_s4_class({ embed(iris[1:4], "PCA_L1", ndim = 1,
                            .mute = c("message", "output")) },
                    "dimRedResult")
})
