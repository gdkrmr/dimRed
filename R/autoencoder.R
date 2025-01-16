#' AutoEncoder
#'
#' An S4 Class implementing an Autoencoder
#'
#' Autoencoders are neural networks that try to reproduce their input. Consider
#' this method unstable, as the internals may still be changed.
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
#' emb <- embed(dat, "AutoEncoder")
#'
#' # predicting is possible:
#' samp <- sample(floor(nrow(dat) / 10))
#' emb2 <- embed(dat[samp])
#' emb3 <- predict(emb2, dat[-samp])
#'
#' plot(emb, type = "2vars")
#' plot(emb2, type = "2vars")
#' points(getData(emb3))
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
                   learning_rate = 0.15,
                   loss          = "mse",
                   optimizer     = "adam",
                   encoder       = NULL,
                   decoder       = NULL,
                   ## is.na() of an S4 class gives a warning
                   batchsize     = 20,
                   epochs        = 5,
                   validation_split = 0.2),

    fun = function (data, pars,
                    keep.org.data = TRUE) {
      chckpkg("keras3")
      indata <- data@data
      indims <- ncol(indata)
      ndim <- pars$ndim

      input_data <- keras3::layer_input(shape = indims)
      input_hidden <- keras3::layer_input(shape = ndim)

      ## if the user did not specify encoder and decoder, we build them from the
      ## parameters
      if (is.null(pars$encoder) || is.null(pars$decoder)) {
        depth <- length(pars$n_hidden)
        if (depth %% 2 == 0) {
          stop("the number of layers must be impair")
        }

        if (ndim != pars$n_hidden[ceiling(depth / 2)]) {
          stop("the middle of n_hidden must be equal to ndim")
        }

        if (depth != length(pars$activation)) {
          stop("declare an activation function for each layer")
        }


        in_depth <- ceiling(depth / 2)
        out_depth <- depth - in_depth


        layers <- mapply(
          function(s, n) keras3::layer_dense(units = n, activation = s),
          pars$activation, pars$n_hidden,
          SIMPLIFY = FALSE
        )

        encoder <- c(input_data, layers[1:in_depth])
        decoder <- c(input_hidden, layers[(in_depth + 1):depth])
      } else {
        encoder <- c(input_data, pars$encoder)
        decoder <- c(input_hidden, pars$decoder)
      }

      ## now build the actual model, compile it and fit it
      autoencoder <- c(input_data, encoder, decoder)
      encoder <- encoder %>%
        chain_list() %>%
        keras3::keras_model()
      decoder <- decoder %>%
        chain_list() %>%
        keras3::keras_model()
      autoencoder <- autoencoder %>%
        chain_list() %>%
        keras3::keras_model()

      autoencoder %>%
        keras3::compile(
          optimizer = pars$optimizer,
          loss = pars$loss
        )

      history <- autoencoder %>%
        keras3::fit(
                 indata, indata,
                 epochs           = pars$epochs,
                 batch_size       = pars$batchsize,
                 validation_split = pars$validation_split,
                 verbose          = 0
               )

      meta <- data@meta
      orgdata <- if (keep.org.data) data@data else NULL
      indata <- data@data

      appl <- function(x) {
        appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
        proj <- if (inherits(x, "dimRedData")) x@data else x

        if (ncol(proj) != ncol(data@data))
          stop("x must have the same number of dimensions ",
               "as the original data")

        res <- encoder(proj)
        colnames(res) <- paste0("AE", seq_len(ncol(res)))
        new("dimRedData", data = res, meta = appl.meta)
      }

      inv <- function(x) {
        appl.meta <- if (inherits(x, "dimRedData")) x@meta else data.frame()
        proj <- if (inherits(x, "dimRedData")) x@data else x

        if (ncol(proj) != pars$ndim)
          stop("x must have the same number of dimensions ",
               "as ndim data")

        res <- decoder(proj)
        colnames(res) <- colnames(indata)
        new("dimRedData", data = res, meta = appl.meta)
      }

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
    },
    requires = c("tensorflow", "keras3"))
)

## no idea why these and variants do not work:
## chain_list <- function(x1, x2) Reduce(`%>%`, x2, init = x1)
## chain_list <- function(x) Reduce(`%>%`, x)
chain_list <- function(x1, x2 = NULL) {

  if (is.null(x2)) {
    stopifnot(is.list(x1))
    result <- x1[[1]]
    if (length(x1) > 1) for (i in 2:length(x1)) {
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
