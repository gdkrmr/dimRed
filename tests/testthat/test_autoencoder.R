## skip_if_no_tensorflow <- function() {
##   if (!requireNamespace(c("tensorflow", "reticulate"), quietly = TRUE) ||
##         (!reticulate::py_module_available("tensorflow") &&
##          Sys.getenv("BNET_FORCE_AUTOENCODER_TESTS") != "1"))
##     skip("TensorFlow not available for testing")
## }
## skip_if_no_keras <- function() {
##   if (!requireNamespace(c("keras3", "reticulate"), quietly = TRUE) ||
##         (!keras::is_keras_available() &&
##            Sys.getenv("BNET_FORCE_AUTOENCODER_TESTS") != "1"))
##     skip("Keras not available for testing")
## }

## test_that("Check if tensorflow is installed correctly.", {
##   skip_if_no_tensorflow()
##   requireNamespace("tensorflow", quietly = TRUE)
##   tensorflow::tf$compat$v1$disable_v2_behavior()
##   # I have not found a way to suppress the warning tf gives on first use.
##   sess <- tensorflow::tf$compat$v1$Session()
##   hello <- "Hello, TensorFlow!"
##   tf_hello <- tensorflow::tf$compat$v1$constant(hello)
##   tf_hello_res <- sess$run(tf_hello)

##   # in python 3 this returns a `bytes` object $decode() transforms it into a
##   # sting, in python 2 this is a simple string
##   if(!is.character(tf_hello_res))
##     tf_hello_res <- tf_hello_res$decode()

##   ## print("tf_hello_res:")
##   ## print(str(tf_hello_res))
##   ## print(tf_hello_res)

##   expect(tf_hello_res == hello, paste("tensorflow does not work:\n",
##                                       "hello =", hello, "\n",
##                                       "sess$run(tf_hello) =", tf_hello_res))
## })

## test_that("Check errors when building autoencoder.", {
##   skip_if_no_tensorflow()
##   iris_data <- as(iris[, 1:4], "dimRedData")
##   expect_error(embed(iris_data, "AutoEncoder", activation = "sigmoid"),
##                "declare an activation function for each layer")
##   expect_error(embed(iris_data, "AutoEncoder", n_hidden = c(1, 2, 2, 1)),
##                "the number of layers must be impair")
##   expect_error(embed(iris_data, "AutoEncoder", weight_decay = -1),
##                "weight decay must be > 0")
##   expect_error(embed(iris_data, "AutoEncoder", learning_rate = -1),
##                "learning rate must be > 0")
##   expect_error(embed(iris_data, "AutoEncoder", n_steps = -1),
##                "n_steps must be > 0")
##   expect_error(embed(iris_data, "AutoEncoder", n_hidden = c(4, 2, 4), ndim = 3),
##                "the middle of n_hidden must be equal to ndim")
## })


## test_that("using autoencoder with parameters", {
##     skip_if_no_tensorflow()
##     iris_data <- as(iris[, 1:4], "dimRedData")
##     expect_equal(class(iris_data)[1], "dimRedData")

##     ae <- lapply(1:2, function(x) embed(iris_data, "AutoEncoder",
##                                         n_hidden = c(10, x, 10),
##                                         ndim = x,
##                                         n_steps = 100))
##     aq <- lapply(ae, function(x) quality(x, "reconstruction_rmse"))
##     lapply(ae, function(x) expect_s4_class(x, "dimRedResult"))

##     ## expect(aq[[1]] > aq[[2]], "the error should decrease with more dimensions")
##     ## expect(aq[[2]] > aq[[3]], "the error should decrease with more dimensions")
##     ## expect(aq[[3]] > aq[[4]], "the error should decrease with more dimensions")

##     lapply(1:length(ae), function(x) expect_equal(x, getNDim(ae[[x]])))


##     ae <- lapply(1:2, function(x) embed(iris_data,
##                                         "AutoEncoder",
##                                         n_hidden = c(10, x, 10),
##                                         ndim = x,
##                                         weight_decay = 0.1,
##                                         n_steps = 100))
##     aq <- lapply(ae, function(x) quality(x, "reconstruction_rmse"))
##     lapply(ae, function(x) expect_s4_class(x, "dimRedResult"))

##     ## expect(aq[[1]] > aq[[2]], "the error should decrease with more dimensions")
##     ## expect(aq[[2]] > aq[[3]], "the error should decrease with more dimensions")
##     ## expect(aq[[3]] > aq[[4]], "the error should decrease with more dimensions")

##     lapply(1:length(ae), function(x) expect_equal(x, getNDim(ae[[x]])))



##     ae <- lapply(1:2, function(x) embed(iris_data,
##                                         "AutoEncoder",
##                                         n_hidden = c(10, x, 10),
##                                         ndim = x,
##                                         learning_rate = 0.1,
##                                         weight_decay = 0.1,
##                                         n_steps = 100))
##     aq <- lapply(ae, function(x) quality(x, "reconstruction_rmse"))
##     lapply(ae, function(x) expect_s4_class(x, "dimRedResult"))

##     ## expect(aq[[1]] > aq[[2]], "the error should decrease with more dimensions")
##     ## expect(aq[[2]] > aq[[3]], "the error should decrease with more dimensions")
##     ## expect(aq[[3]] > aq[[4]], "the error should decrease with more dimensions")

##     lapply(1:length(ae), function(x) expect_equal(x, getNDim(ae[[x]])))



##     ae <- lapply(1:2, function(x) embed(iris_data,
##                                         "AutoEncoder",
##                                         n_hidden = c(10, x, 10),
##                                         activation = c("sigmoid", "sigmoid", "sigmoid"),
##                                         ndim = x,
##                                         learning_rate = 0.1,
##                                         weight_decay = 0.1,
##                                         n_steps = 100))
##     aq <- lapply(ae, function(x) quality(x, "reconstruction_rmse"))
##     lapply(ae, function(x) expect_s4_class(x, "dimRedResult"))

##     aa <- lapply(c("tanh", "sigmoid", "relu", "elu"),
##                  function(x) embed(iris_data,
##                                    "AutoEncoder",
##                                    n_hidden = c(10, 2, 10),
##                                    activation = c("sigmoid", "sigmoid", "sigmoid"),
##                                    ndim = 2,
##                                    learning_rate = 0.1,
##                                    weight_decay = 0.1,
##                                    n_steps = 100))
##     aaq <- lapply(aa, function(x) quality(x, "reconstruction_rmse"))
##     lapply(aa, function(x) expect_s4_class(x, "dimRedResult"))
##     ## expect(aq[[1]] > aq[[2]], "the error should decrease with more dimensions")
##     ## expect(aq[[2]] > aq[[3]], "the error should decrease with more dimensions")
##     ## expect(aq[[3]] > aq[[4]], "the error should decrease with more dimensions")

##     lapply(1:length(ae), function(x) expect_equal(x, getNDim(ae[[x]])))
## })

## test_that("using autoencoder with autoencoder results", {
##     skip_if_no_tensorflow()

##     tensorflow::tf$compat$v1$set_random_seed(2)
##     iris_data <- as(iris[, 1:4], "dimRedData")
##     expect_equal(class(iris_data)[1], "dimRedData")

##     ae1 <- lapply(1:2, function(x) embed(iris_data, "AutoEncoder",
##                                          n_hidden = c(10, x, 10),
##                                          ndim = x, n_steps = 1))
##     aq1 <- lapply(ae1, function(x) quality(x, "reconstruction_rmse"))

##     ae2 <- lapply(ae1, function(x) embed(iris_data, "AutoEncoder",
##                                          autoencoder = x, n_steps = 1000))
##     aq2 <- lapply(ae2, function(x) quality(x, "reconstruction_rmse"))

##     lapply(ae1, function(x) expect_s4_class(x, "dimRedResult"))
##     lapply(ae2, function(x) expect_s4_class(x, "dimRedResult"))

##     expect(aq1[[1]] > aq2[[1]], "the error should decrease with more steps")
##     expect(aq1[[2]] > aq2[[2]], "the error should decrease with more steps")
##     ## expect(aq1[[3]] > aq2[[3]], "the error should decrease with more steps")
##     ## expect(aq1[[4]] > aq2[[4]], "the error should decrease with more steps")

##     lapply(1:length(ae1), function(x) expect_equal(x, getNDim(ae1[[x]])))
##     lapply(1:length(ae2), function(x) expect_equal(x, getNDim(ae2[[x]])))
## })

## test_that("using autoencoder with keras", {
##   skip_if_no_tensorflow()
##   skip_if_no_keras()

##   encoder <- function(i) list(keras::layer_dense(units = 10,
##                                                  activation = "tanh"),
##                               keras::layer_dense(units = i))
##   decoder <- function() list(keras::layer_dense(units = 10,
##                                                 activation = "tanh"),
##                              keras::layer_dense(units = 4))

##   iris_data <- as(iris[, 1:4], "dimRedData")

##   ae1 <- lapply(1:2, function(x) embed(iris_data, "AutoEncoder",
##                                        keras_graph = list(encoder = encoder(x),
##                                                           decoder = decoder()),
##                                        n_steps = 2))
##   aq1 <- lapply(ae1, function(x) quality(x, "reconstruction_rmse"))

##   ae2 <- lapply(ae1, function(x) embed(iris_data, "AutoEncoder",
##                                        autoencoder = x))
##   aq2 <- lapply(ae2, function(x) quality(x, "reconstruction_rmse"))

##   lapply(ae1, function(x) expect_s4_class(x, "dimRedResult"))
##   lapply(ae2, function(x) expect_s4_class(x, "dimRedResult"))

##   ## expect(aq1[[1]] > aq2[[1]], "the error should decrease with more steps")
##   ## expect(aq1[[2]] > aq2[[2]], "the error should decrease with more steps")
##   ## expect(aq1[[3]] > aq2[[3]], "the error should decrease with more steps")
##   ## expect(aq1[[4]] > aq2[[4]], "the error should decrease with more steps")

##   lapply(1:length(ae1), function(x) expect_equal(x, getNDim(ae1[[x]])))
##   lapply(1:length(ae2), function(x) expect_equal(x, getNDim(ae2[[x]])))

## })



## ## test_that("garbage collection", {
## ##   skip_if_no_tensorflow()
## ##   tmp <- tf$get_session_handle(environment(ae[[1]]@apply)$dec)

## ##   tmp <- tf$get_default_session()

## ##   tmp$close
## ##   tmp
## ##   tf$get_session_handle()
## ##   tf$Session()
## ## })
