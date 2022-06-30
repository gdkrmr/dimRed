test_that("squared euclidean distance", {
    a <- matrix(rnorm(25), 5, 5)
    b <- matrix(rnorm(25), 5, 5)

    expect_equal(
        t(as.matrix(dist(rbind(a, b)))[6:10, 1:5] ^ 2),
        pdist2(a, b),
        ignore_attr = TRUE
    )
})

test_that("formula functions", {
    a <- matrix(rnorm(25), 5, 5)
    b <- matrix(rnorm(25), 5, 5)

    expect_true(rhs(a + b ~ c + d) == ~ c + d + 0)
    expect_true(lhs(a + b ~ c + d) == ~ a + b + 0)
    ## expect_equal(rhs(a + b ~ c + d), ~ c + d + 0)
    ## expect_equal(lhs(a + b ~ c + d), ~ a + b + 0)
})



test_that("makeEpsGraph", {
    check_makeEpsGraph <- function(x, eps){
        naive <- as.matrix(dist(x))
        naive[naive >= eps] <- 0
        epsSp <- as.matrix(makeEpsSparseMatrix(x, eps))
        all(naive == epsSp)
    }
    expect_true(check_makeEpsGraph(iris[1:4], 1000))
    expect_true(check_makeEpsGraph(iris[1:4], 1))
    expect_true(check_makeEpsGraph(iris[1:4], 0.5))
})


test_that("getRotationMatrixFail", {
  if(requireNamespace("Rtsne"))
    skip("Rtsne not available")

  irisData <- as(iris[, 1:4], "dimRedData")
  expect_equal(class(irisData)[1], "dimRedData")

  irisRes <- embed(irisData, "tSNE")

  expect_error(getRotationMatrix(irisRes), "Not implemented for")
})
