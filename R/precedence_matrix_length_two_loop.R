#' Length Two Loop Precedence Matrix
#'
#' Construct a precedence matrix counting how often pattern `aba` occurs.
#'
#' @param eventlog The event log object to be used.
#'
#' @examples
#' m <- precedence_matrix_length_two_loops(hospital_multi_perspective)
#' print(m)
#' as.matrix(m)
#'
#' @export precedence_matrix_length_two_loops
precedence_matrix_length_two_loops <- function(eventlog) {
  stopifnot("eventlog" %in% class(eventlog))

  eventlog <- reduce_simple_eventlog(eventlog)
  precedence_matrix_length_two_loops_impl(eventlog)
}

precedence_matrix_length_two_loops_impl <- function(simplelog) {
  mat <- as_tibble(count_length_two_loops(simplelog$case_id,
                                          simplelog$activity_id))

  class(mat) <- c("process_matrix", class(mat))
  type <- "absolute"
  attr(type, "perspective") <- "frequency_length_two_loops"
  attr(mat, "matrix_type") <- type
  return(mat)
}

