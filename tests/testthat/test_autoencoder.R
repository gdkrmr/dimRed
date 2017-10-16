

skip_if_no_tensorflow <- function() {
  if (!reticulate::py_module_available("tensorflow"))
    skip("TensorFlow not available for testing")
}

context("AutoEncoder")

test_that("using autoencoder with parameters", {
    skip_if_no_tensorflow()
    iris_data <- as(iris[, 1:4], "dimRedData")
    expect_equal(class(iris_data)[1], "dimRedData")

    ae <- lapply(1:4, function(x) embed(iris_data, "AutoEncoder", n_hidden = c(10, x, 10), ndim = x))
    aq <- lapply(ae, function(x) quality(x, "reconstruction_rmse"))
    lapply(ae, function(x) expect_s4_class(x, "dimRedResult"))

    ## expect(aq[[1]] > aq[[2]], "the error should decrease with more dimensions")
    ## expect(aq[[2]] > aq[[3]], "the error should decrease with more dimensions")
    ## expect(aq[[3]] > aq[[4]], "the error should decrease with more dimensions")

    lapply(1:length(ae), function(x) expect_equal(x, getNDim(ae[[x]])))
})


test_that("using autoencoder with autoencoder results", {
    skip_if_no_tensorflow()
    iris_data <- as(iris[, 1:4], "dimRedData")
    expect_equal(class(iris_data)[1], "dimRedData")

    ae1 <- lapply(1:4, function(x) embed(iris_data, "AutoEncoder", n_hidden = c(10, x, 10), ndim = x, n_steps = 2))
    aq1 <- lapply(ae1, function(x) quality(x, "reconstruction_rmse"))

    ae2 <- lapply(ae1, function(x) embed(iris_data, "AutoEncoder", autoencoder = x))
    aq2 <- lapply(ae2, function(x) quality(x, "reconstruction_rmse"))

    lapply(ae1, function(x) expect_s4_class(x, "dimRedResult"))
    lapply(ae2, function(x) expect_s4_class(x, "dimRedResult"))

    expect(aq1[[1]] > aq2[[1]], "the error should decrease with more steps")
    expect(aq1[[2]] > aq2[[2]], "the error should decrease with more steps")
    expect(aq1[[3]] > aq2[[3]], "the error should decrease with more steps")
    expect(aq1[[4]] > aq2[[4]], "the error should decrease with more steps")

    lapply(1:length(ae1), function(x) expect_equal(x, getNDim(ae1[[x]])))
    lapply(1:length(ae2), function(x) expect_equal(x, getNDim(ae2[[x]])))
})

## test_that("garbage collection", {
##   skip_if_no_tensorflow()
##   tmp <- tf$get_session_handle(environment(ae[[1]]@apply)$dec)

##   tmp <- tf$get_default_session()

##   tmp$close
##   tmp
##   tf$get_session_handle()
##   tf$Session()
## })
