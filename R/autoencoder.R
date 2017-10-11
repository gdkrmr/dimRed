#' AutoEncoder
#'
#' An S4 Class implementing an Autoencoder
#'
#' Autoencoders are neural networks that try to reproduce their input.
#'
#' @template dimRedMethodSlots
#'
#' @template dimRedMethodGeneralUsage
#'
#' @section Parameters:
#' Autoencoder can take the following parameters:
#' \describe{
#'   \item{ndim}{The number of dimensions for reduction.}
#'   \item{n_hidden}{The number of neurons in the hidden
#'       layers, the length specifies the number of layers,
#'       the length must be impair, the middle number must
#'       be the same as ndim.}
#'   \item{activation}{The activation functions for the layers,
#'       one of "tanh", "sigmoid", "relu", "elu", everything
#'       else will silently be ignored and there will be no
#'       activation function for the layer.}
#'   \item{weight_decay}{the coefficient for weight decay,
#'       set to 0 if no weight decay desired.}
#'   \item{learning_rate}{The learning rate for gradient descend}
#'   \item{batchsize}{If NA, all data will be used for training,
#'       else only a random subset of size batchsize will be used}
#'   \item{n_steps}{the number of training steps.}
#' }
#'
#' @section Implementation:
#' Uses \pkg{tensorflow} as a backend, for details an
#'   problems relating tensorflow, see \url{https://tensorflow.rstudio.com}.
#' Currently only a very simple Autoencoder is implemented.
#'
#' #@references
#'
#' @examples
#' dat <- loadDataSet("3D S Curve")
#'
#' ## use the S4 Class directly:
#' autoenc <- AutoEncoder()
#' emb <- autoenc@fun(dat, autoenc@stdpars)
#'
#' ## simpler, use embed():
#' emb2 <- embed(dat, "AutoEncoder")
#'
#' plot(emb, type = "2vars")
#'
#' samp <- sample(floor(nrow(dat) / 10))
#' embsamp <- autoenc@fun(dat[samp], autoenc@stdpars)
#' embother <- embsamp@apply(dat[-samp])
#' plot(embsamp, type = "2vars")
#' points(embother)
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' @family dimensionality reduction methods
#' @export AutoEncoder
#' @exportClass AutoEncoder
AutoEncoder <- setClass(
    "AutoEncoder",
    contains  = "dimRedMethod",
    prototype = list(
        stdpars = list(ndim       = 2,
                       n_hidden   = c(10, 2, 10),
                       activation = c("tanh", "lin", "tanh"),
                       weight_decay = 0.001,
                       learning_rate = 0.15,
                       batchsize = NA,
                       n_steps = 500),
        fun     = function (data, pars,
                            keep.org.data = TRUE) {
        chckpkg("tensorflow")

        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        indata <- data@data

        with(pars, {
          if (length(n_hidden) != length(activation))
            stop("declare an activation for each layer")
          if (length(n_hidden) %% 2 == 0)
            stop("the number of layers must be impair")
          if (weight_decay < 0)
            stop("weight decay must be > 0")
          if (learning_rate <= 0)
            stop("learning rate must be > 0")
          if (n_steps <= 0)
            stop("n_steps must be > 0")
          if (ndim != n_hidden[ceiling(length(n_hidden) / 2)])
            stop("the middle of n_hidden must be equal to ndim")
        })

        tf <- tensorflow::tf
        n_hidden <- pars$n_hidden
        activation <- pars$activation
        n_in <- ncol(indata)

        get_activation_function <- function(x) {
          switch(
            x,
            tanh    = tf$tanh,
            sigmoid = tf$sigmoid,
            relu    = tf$nn$relu,
            elu     = tf$elu,
            I
          )
        }


        input <- tf$placeholder("float", shape = tensorflow::shape(NULL, n_in),      name = "data")
        indec <- tf$placeholder("float", shape = tensorflow::shape(NULL, pars$ndim), name = "nlpca")

        w <- lapply(seq_len(length(n_hidden) + 1), function(x) {
          n1 <- if (x == 1)               n_in else n_hidden[x - 1]
          n2 <- if (x > length(n_hidden)) n_in else n_hidden[x]
          tf$Variable(tf$random_uniform(tensorflow::shape(n1, n2), 1.0, -1.0), name = paste0("w_", x))
        })
        b <- lapply(seq_len(length(n_hidden) + 1), function (x) {
          n <- if (x > length(n_hidden)) n_in else n_hidden[x]
          tf$Variable(tf$zeros(tensorflow::shape(n)), name = paste0("b_", x))
        })

        enc <- input
        for (i in 1:ceiling(length(n_hidden) / 2)) {
          sigma <- get_activation_function(activation[i])
          enc <- sigma(tf$matmul(enc, w[[i]]) + b[[i]])
        }

        dec <- indec
        for (i in (ceiling(length(n_hidden) / 2) + 1):(length(n_hidden) + 1)) {
          sigma <- get_activation_function(activation[i])
          dec <- sigma(tf$matmul(dec, w[[i]]) + b[[i]])
        }

        encdec <- enc
        for (i in (ceiling(length(n_hidden) / 2) + 1):(length(n_hidden) + 1)) {
          sigma <- get_activation_function(activation[i])
          encdec <- sigma(tf$matmul(encdec, w[[i]]) + b[[i]])
        }

        loss <- Reduce(`+`, lapply(w, function (x) tf$reduce_sum(tf$pow(x, 2))), 0)
        loss <- Reduce(`+`, lapply(b, function (x) tf$reduce_sum(tf$pow(x, 2))), loss)
        loss <- tf$reduce_mean((encdec - input) ^ 2) + pars$weight_decay * loss
        optimizer <- tf$train$GradientDescentOptimizer(pars$learning_rate)
        train <- optimizer$minimize(loss)

        ## TODO: how does garbage collection work here? will this be around
        ## forever? or may the graph just disappear from the function
        ## environments?
        sess <- tf$Session()
        sess$run(tf$global_variables_initializer())

        make_feed <- function(data, samples) {
          tensorflow::dict(
            input = if (is.na(pars$batchsize))
                      data
                    else
                      data[sample(seq_len(nrow(data)), batchsize), ]
          )
        }

        ## feed <- dict(input = in_data) #[sort(sample(1:nrow(in_data), batchsize)), ])
        ## cat("0 -", sess$run(loss, feed_dict = feed), "\n")
        for (step in 1:pars$n_steps) {
          sess$run(train, feed_dict = make_feed(indata, pars$batchsize))
          ## cat(step, "-", sess$run(loss, feed_dict = feed), "\n")
        }

        outdata <- sess$run(enc, feed_dict = tensorflow::dict(input = indata))

        appl <- function(x) {
          appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
          proj <- if (inherits(x, "dimRedData")) x@data else x

          if (ncol(proj) != ncol(data@data))
            stop("x must have the same number of dimensions ",
                 "as the original data")

          res <- sess$run(enc, feed_dict = tensorflow::dict(input = proj))
          colnames(res) <- paste0("nlPCA", seq_len(ncol(res)))
          return(res)
        }

        inv <- function(x) {
          appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
          proj <- if (inherits(x, "dimRedData")) x@data else x

          if (ncol(proj) != pars$ndim)
            stop("x must have the same number of dimensions ",
                 "as ndim data")

          res <- sess$run(dec, feed_dict = tensorflow::dict(indec = proj))
          colnames(res) <- colnames(indata)
          return(res)
        }



        colnames(outdata) <- paste0("nlPCA", seq_len(ncol(outdata)))

        return(new(
          "dimRedResult",
          data = new("dimRedData",
                     data = outdata,
                     meta = meta),
          org.data     = orgdata,
          apply        = appl,
          inverse      = inv,
          has.apply    = TRUE,
          has.inverse  = TRUE,
          has.org.data = keep.org.data,
          method       = "AutoEncoder",
          pars         = pars
        ))
        })
)
