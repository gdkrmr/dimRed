
#' An S4 class that makes sure that the wrappers for dimensionality
#' reduction work.
#'
#' @slot fun function call
#' 
#' @export
setClass('dimRedMethod',slots = c(fun = 'function'))

