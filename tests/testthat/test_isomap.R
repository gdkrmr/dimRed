

context("isomap")

## no isomap specific tests, because forward method is not really
## exact.


test_that("check vs vegan isomap", {

    eps <- 1e-13
    a <- loadDataSet("3D S Curve", n = 200)

    vegiso <- vegan::isomap(dist(getData(a)), k = 8, ndim = 2)
    vegy <- vegan::scores(vegiso)

    drdiso <- embed(a, "Isomap", knn = 8, ndim = 2)
    drdy <- drdiso@data@data

    ## Randomly fails:
    ## expect_equivalent(drdy, vegy)

    err1 <- max(abs(drdy - vegy))
    drdy[, 2] <- -drdy[, 2]
    err2 <- max(abs(drdy - vegy))
    err <- min(err1, err2)

    expect_true(err < eps, info = paste0("err = ", err,
                                         ", eps = ", eps,
                                         ", expected err < eps"))

})
