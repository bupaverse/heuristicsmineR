
#' Parallel Matrix with Lifecycle
#'
#' @param eventlog The event log object to be used.
#'
#' @examples
#' parallel_matrix_lifecycle(L_heur_1)
#'
#' @export
parallel_matrix_lifecycle <- function(eventlog) {
  stopifnot("eventlog" %in% class(eventlog))

  activitylog <- reduce_activitylog(eventlog)
  parallel_matrix_lifecycle_impl(activitylog)
}

parallel_matrix_lifecycle_impl <- function(activitylog) {
  mat <- as_tibble(count_parallel_lifecycle(activitylog$case_id,
                                            activitylog$activity_id,
                                            activitylog$lifecycle))

  class(mat) <- c("process_matrix", class(mat))
  type <- "absolute"
  attr(type, "perspective") <- "frequency_parallel_lifecycle"
  attr(mat, "matrix_type") <- type
  return(mat)
}
