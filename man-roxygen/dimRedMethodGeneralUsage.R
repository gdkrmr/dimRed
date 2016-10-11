#' @section General usage:
#' Dimensionality reduction methods are S4 Classes that either be used
#' directly, in which case they have to be initialized and a full
#' list with parameters has to be handed to the \code{@@fun()}
#' slot, or the method name be passed to the embed function and
#' parameters can be given to the \code{...}, in which case
#' missing parameters will be replaced by the ones in the
#' \code{@@stdpars}.
#' 
