

context("isomap")

## no isomap specific tests, because forward method is not really
## exact.

a <- matrix(rnorm(1000), 100, 10)

vegiso <- vegan::isomap(dist(a), k = 10)
vegy <- vegan::scores(vegiso)

drdiso <- embed(a, "isomap", knn = 10, ndim = 10)
drdy <- drdiso@data@data

test_that("check vs vegan isomap", {
    expect_equivalent(drdy, vegy)
})
