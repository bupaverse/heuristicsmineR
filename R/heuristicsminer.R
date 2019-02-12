#' @useDynLib heuristicsmineR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("heuristicsmineR", libpath)
}
