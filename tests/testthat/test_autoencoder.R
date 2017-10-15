
context("AutoEncoder")

test_that("using the autoencoder", {
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



## test_that("garbage collection", {
##   tmp <- tf$get_session_handle(environment(ae[[1]]@apply)$dec)

##   tmp <- tf$get_default_session()

##   tmp$close
##   tmp
##   tf$get_session_handle()
##   tf$Session()
## })
