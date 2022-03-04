
## no isomap specific tests, because forward method is not really
## exact.


test_that("check vs vegan isomap", {

    eps <- 1e-8
    a <- loadDataSet("3D S Curve", n = 200)

    vegiso <- vegan::isomap(dist(getData(a)), k = 8, ndim = 2)
    vegy <- vegan::scores(vegiso)

    drdiso <- embed(a, "Isomap", knn = 8, ndim = 2)
    drdy <- drdiso@data@data

    ## Randomly fails:
    ## expect_equal(drdy, vegy, ignore_attr = TRUE)

    err1 <- max(abs(drdy - vegy))

    drdy[, 2] <- -drdy[, 2]
    err2 <- max(abs(drdy - vegy))

    drdy[, 1] <- -drdy[, 1]
    err3 <- max(abs(drdy - vegy))

    drdy[, 2] <- -drdy[, 2]
    err4 <- max(abs(drdy - vegy))

    err <- min(err1, err2, err3, err4)

    expect_true(err < eps, info = paste0("err = ", err,
                                         ", eps = ", eps,
                                         ", expected err < eps"))

})


test_that("check other.data", {
  a <- loadDataSet("3D S Curve", n = 200)
  drdiso <- embed(a, "Isomap", knn = 8, ndim = 2, get_geod = TRUE)
  expect_true(inherits(getOtherData(drdiso)$geod, "dist"))
})
