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
#'   \item{graph}{Optional: A list of bits and pieces that define the
#'      autoencoder in tensorflow, see details.}
#'   \item{keras_graph}{Optional: A list of keras layers that define
#'      the encoder and decoder, specifying this, will ignore all
#'      other topology related variables, see details.}
#'   \item{batchsize}{If NA, all data will be used for training,
#'       else only a random subset of size batchsize will be used}
#'   \item{n_steps}{the number of training steps.}
#' }
#'
#' @section Details:
#' There are several ways to specify an autoencoder, the simplest is to pass the
#' number of neurons per layer in \code{n_hidden}, this must be a vector of
#' integers of impair length and it must be symmetric and the middle number must
#' be equal to \code{ndim}, For every layer an activation function can be
#' specified with \code{activation}.
#'
#' For regularization weight decay can be specified by setting
#' \code{weight_decay} > 0.
#'
#' Currently only a gradient descent optimizer is used, the learning rate can be
#' specified by setting \code{learning_rate}.
#' The learner can operate on batches if \code{batchsize} is not \code{NA}.
#' The number of steps the learner uses is specified using \code{n_steps}.
#'
#' @section Further training a model:
#' If the model did not converge in the first training phase or training with
#' different data is desired, the \code{\link{dimRedResult}} object may be
#' passed as \code{autoencoder} parameter; In this case all topology related
#' parameters will be ignored.
#'
#' @section Using Keras layers:
#' The encoder and decoder part can be specified using a list of \pkg{keras}
#' layers. This requires a list with two entries, \code{encoder} should contain
#' a LIST of keras layers WITHOUT the \code{\link[keras]{layer_input}}
#' that will be concatenated in order to form the encoder part.
#' \code{decoder} should be
#' defined accordingly, the output of \code{decoder} must have the same number
#' of dimensions as the input data.
#'
#' @section Using Tensorflow:
#' The model can be entirely defined in \pkg{tensorflow}, it must contain a
#' list with the following entries:
#' \describe{
#'   \item{encoder}{A tensor that defines the encoder.}
#'   \item{decoder}{A tensor that defines the decoder.}
#'   \item{network}{A tensor that defines the reconstruction (encoder + decoder).}
#'   \item{loss}{A tensor that calculates the loss (network + loss function).}
#'   \item{in_data}{A \code{placeholder} that points to the data input of
#'     the network AND the encoder.}
#'   \item{in_decoder}{A \code{placeholder} that points to the input of
#'     the decoder.}
#'   \item{session}{A \pkg{tensorflow} \code{Session} object that holds
#'     the values of the tensors.}
#' }
#'
#' @section Implementation:
#' Uses \pkg{tensorflow} as a backend, for details an
#'   problems relating tensorflow, see \url{https://tensorflow.rstudio.com}.
#'
#' @examples
#' \dontrun{
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
#' points(embother@data)
#' }
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
        stdpars = list(ndim          = 2,
                       n_hidden      = c(10, 2, 10),
                       activation    = c("tanh", "lin", "tanh"),
                       weight_decay  = 0.001,
                       learning_rate = 0.15,
                       graph         = NULL,
                       keras_graph   = NULL,
                       ## is.na() of an S4 class gives a warning
                       autoencoder   = NULL,
                       batchsize     = NA,
                       n_steps       = 500),
        fun     = function (data, pars,
                            keep.org.data = TRUE) {
        chckpkg("tensorflow")

        meta <- data@meta
        orgdata <- if (keep.org.data) data@data else NULL
        indata <- data@data

        graph <-
            if (!is.null(pars$graph)) {
                message("using predefined graph, ",
                        "ignoring other parameters that define topology, ",
                        "be sure to set ndim to the correct value ",
                        "else you might run into trouble.")
                pars$graph
            } else if (!is.null(pars$autoencoder)) {
                message("using predefined autoencoder object, ",
                        " ignoring other parameters that define topology.")
                if (!(inherits(pars$autoencoder, "dimRedResult") &&
                      pars$autoencoder@method == "AutoEncoder"))
                    stop("autoencoder must be NULL, ",
                         "or of type dimRedResult by an AutoEncoder object.")

                ## setting topology related parameters from autoencoder
                pars$ndim       <- pars$autoencoder@pars$ndim
                pars$n_hidden   <- pars$autoencoder@pars$n_hidden
                pars$activation <- pars$autoencoder@pars$activation

                pars$autoencoder@pars$graph
            } else if (!is.null(pars$keras_graph)) {
              message("using predefined keras graph, ",
                      "ignoring parameters that define topology")
              tmp <- graph_keras(encoder = pars$keras_graph$encoder,
                                 decoder = pars$keras_graph$decoder,
                                 n_in    = ncol(indata))

              pars$ndim <- tmp$encoder$shape$dims[[2]]$value

              tmp
            } else {
                with(pars, {
                    graph_params(
                        d_in          = ncol(indata),
                        n_hidden      = n_hidden,
                        activation    = activation,
                        weight_decay  = weight_decay,
                        learning_rate = learning_rate,
                        n_steps       = n_steps,
                        ndim          = ndim
                    )
                })
            }

        if (!"encoder"    %in% names(graph)) stop("no encoder in graph")
        if (!"decoder"    %in% names(graph)) stop("no decoder in graph")
        if (!"network"    %in% names(graph)) stop("no network in graph")
        if (!"loss"       %in% names(graph)) stop("no loss in graph")
        if (!"in_decoder" %in% names(graph)) stop("no in_decoder in graph")
        if (!"in_data"    %in% names(graph)) stop("no in_data in graph")
        if (!"session"    %in% names(graph)) stop("no session in graph")

        ## TODO: I am not sure if there is a way to do this directly on the list
        ## objects
        graph_data_input    <- graph$in_data
        graph_decoder_input <- graph$in_dec
        sess                <- graph$session

        optimizer <-
            tensorflow::tf$train$GradientDescentOptimizer(pars$learning_rate)
        train <- optimizer$minimize(graph$loss)

        ## TODO: do proper batching and hold out
        for (step in 1:pars$n_steps) {
            sess$run(train, feed_dict =
                tensorflow::dict(
                    graph_data_input =
                        if (is.na(pars$batchsize)) {
                            indata
                        } else {
                            indata[
                                sample(seq_len(nrow(indata)), pars$batchsize),
                            ]
                        }
                )
            )
        }

        outdata <-
            sess$run(graph$encoder,
                     feed_dict = tensorflow::dict(graph_data_input = indata))

        appl <- function(x) {
            appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
            proj <- if (inherits(x, "dimRedData")) x@data else x

            if (ncol(proj) != ncol(data@data))
                stop("x must have the same number of dimensions ",
                     "as the original data")

            res <-
                sess$run(graph$encoder,
                         feed_dict = tensorflow::dict(graph_data_input = proj))

            colnames(res) <- paste0("AE", seq_len(ncol(res)))

            new("dimRedData", data = res, meta = appl.meta)
        }

        inv <- function(x) {
            appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
            proj <- if (inherits(x, "dimRedData")) x@data else x

            if (ncol(proj) != pars$ndim)
                stop("x must have the same number of dimensions ",
                     "as ndim data")

            res <- sess$run(
              graph$decoder,
              feed_dict = tensorflow::dict(
                graph_decoder_input = proj
            ))

            colnames(res) <- colnames(indata)

            new("dimRedData", data = res, meta = appl.meta)
        }

        ## TODO: this is a hack and there should be an "official" way to save
        ## extra data in a dimRedResult object
        pars$graph <- graph

        colnames(outdata) <- paste0("AE", seq_len(ncol(outdata)))

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

get_activation_function <- function(x) {
    switch(
        x,
        tanh    = tensorflow::tf$tanh,
        sigmoid = tensorflow::tf$sigmoid,
        relu    = tensorflow::tf$nn$relu,
        elu     = tensorflow::tf$elu,
        I
    )
}

## no idea why these and variants do not work:
## chain_list <- function(x1, x2) Reduce(`%>%`, x2, init = x1)
## chain_list <- function(x) Reduce(`%>%`, x)
chain_list <- function (x1, x2 = NULL) {

  if(is.null(x2)) {
    stopifnot(is.list(x1))
    result <- x1[[1]]
    if(length(x1) > 1) for (i in 2:length(x1)) {
      result <- result %>% (x1[[i]])
    }
  } else {
    stopifnot(is.list(x2))
    result <- x1
    for (i in 1:length(x2)) {
      result <- result %>% (x2[[i]])
    }
  }

  return(result)
}

graph_keras <- function(encoder, decoder, n_in) {
  chckpkg("keras")
  chckpkg("tensorflow")

  inenc <- keras::layer_input(shape = n_in)
  enc <- inenc %>% chain_list(encoder)

  ndim <- enc$shape$dims[[2]]$value
  indec <- keras::layer_input(shape = ndim)
  dec <- indec %>% chain_list(decoder)

  encdec <- inenc %>% chain_list(encoder) %>% chain_list(decoder)

  ## TODO: check if this uses weight decay, probably not:
  loss <- tensorflow::tf$reduce_mean((encdec - inenc) ^ 2)

  sess <- keras::backend()$get_session()

  return(list(
    encoder    = enc,
    decoder    = dec,
    network    = encdec,
    loss       = loss,
    in_data    = inenc,
    in_decoder = indec,
    session    = sess
  ))
}

graph_params <- function (
    d_in,
    n_hidden,
    activation,
    weight_decay,
    learning_rate,
    n_steps,
    ndim
) {

    if (length(n_hidden) %% 2 == 0)
      stop("the number of layers must be impair")
    if (ndim != n_hidden[ceiling(length(n_hidden) / 2)])
      stop("the middle of n_hidden must be equal to ndim")
    if (length(n_hidden) != length(activation))
      stop("declare an activation function for each layer:",
           "\nn_hidden: ", paste(n_hidden, collapse = " "),
           "\nactivation functions: ", paste(activation, collapse = " "))
    if (weight_decay < 0)
        stop("weight decay must be > 0")
    if (learning_rate <= 0)
        stop("learning rate must be > 0")
    if (n_steps <= 0)
        stop("n_steps must be > 0")

    tf <- tensorflow::tf

    input <- tf$placeholder(
        "float", shape = tensorflow::shape(NULL, d_in),
        name = "input"
    )
    indec <- tf$placeholder(
        "float",
        shape = tensorflow::shape(NULL, ndim),
        name = "nlpca"
    )

    w <- lapply(seq_len(length(n_hidden) + 1), function(x) {
        n1 <- if (x == 1)               d_in else n_hidden[x - 1]
        n2 <- if (x > length(n_hidden)) d_in else n_hidden[x]
        tf$Variable(tf$random_uniform(tensorflow::shape(n1, n2), 1.0, -1.0),
                    name = paste0("w_", x))
    })
    b <- lapply(seq_len(length(n_hidden) + 1), function (x) {
        n <- if (x > length(n_hidden)) d_in else n_hidden[x]
        tf$Variable(tf$zeros(tensorflow::shape(n)),
                    name = paste0("b_", x))
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
    loss <- tf$reduce_mean((encdec - input) ^ 2) + weight_decay * loss

    sess <- tensorflow::tf$Session()
    ## This closes sess if it is garbage collected.
    reg.finalizer(sess, function(x) x$close())
    sess$run(tensorflow::tf$global_variables_initializer())

    return(list(
      encoder    = enc,
      decoder    = dec,
      network    = encdec,
      loss       = loss,
      in_data    = input,
      in_decoder = indec,
      session    = sess
    ))
}
