context("misc functions")


a <- matrix(rnorm(25), 5, 5)
b <- matrix(rnorm(25), 5, 5)


test_that("squared euclidean distance", {
    expect_equivalent(
        t(as.matrix(dist(rbind(a, b)))[6:10, 1:5] ^ 2),
        pdist2(a, b)
    )
})

test_that("formula functions", {
    expect_equal(rhs(a + b ~ c + d), ~ c + d + 0)
    expect_equal(lhs(a + b ~ c + d), ~ a + b + 0)
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
