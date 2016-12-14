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
