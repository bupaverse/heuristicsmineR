#' Create a dependency matrix
#'
#' Creates a dependency matrix from a precedence matrix (\code{\link{precedence_matrix}}) based on different approaches.
#'
#' @param eventlog A bupaR event log, may be NULL when a precedence matrix is provided.
#' @param dependency_type Which approach to use for calculation of the dependency matrix. Currently only (\code{\link{dependency_type_fhm}}) is available.
#' @param threshold A dependency threshold, usually in the interval `[0,1]`, filtering out dependencies below the threshold.
#' @param threshold_frequency An absolute frequency threshold filtering dependencies which are observed infrequently.
#' @param ... Parameters forwarded to (\code{\link{dependency_type_fhm}}).
#'
#' @return A square matrix with class `dependency_matrix` containing the computed dependency values between all activities.
#'
#' @seealso \code{\link{precedence_matrix}}
#'
#' @examples
#' d <- dependency_matrix(L_heur_1)
#' print(d)
#' as.matrix(d)
#'
#' @import dplyr
#' @import bupaR
#' @import processmapR
#' @importFrom stats na.omit
#'
#' @export
dependency_matrix <- function(eventlog = NULL,
                              dependency_type = dependency_type_fhm(threshold_dependency = threshold,
                                                                    threshold_frequency = threshold_frequency,
                                                                    ...),
                              threshold = 0.9,
                              threshold_frequency = 0,
                              ...) {

  if (!("dependency_type" %in% class(dependency_type))) {
    stop("Input parameter `dependency_type` needs to be a dependency type function. For example created with `dependency_type_fhm`.")
  }

  computeFun <- attr(dependency_type, "compute")

  mat <- computeFun(eventlog)

  class(mat) <- c("dependency_matrix", class(mat))

  mat
}
