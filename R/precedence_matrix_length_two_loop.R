#' Length Two Loop Precedence Matrix
#'
#' Construct a precedence matrix counting how often pattern `aba` occurs.
#'
#' @param eventlog The event log object to be used.
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(traffic_fines)
#' m <- precedence_matrix_length_two_loop(traffic_fines)
#' print(m)
#' as.matrix(m)
#' }
#'
#' @export precedence_matrix_length_two_loops
precedence_matrix_length_two_loops <- function(eventlog) {
  stopifnot("eventlog" %in% class(eventlog))

  eventlog <- reduce_simple_eventlog(eventlog)
  precedence_matrix_length_two_loops_impl(eventlog)
}

precedence_matrix_length_two_loops_impl <- function(simplelog) {
  mat <- as_tibble(count_length_two_loops(simplelog[[case_id(simplelog)]],
                                          simplelog[[activity_id(simplelog)]]))

  class(mat) <- c("precedence-matrix", class(mat))
  attr(mat, "matrix_type") <- "absolute"
  return(mat)
}

