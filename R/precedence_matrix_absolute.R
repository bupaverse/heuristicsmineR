#' Precendence Matrix
#'
#' Construct a precendence matrix, showing how activities are followed by each other.
#' This function computes the precedence matrix directly in C++ for efficiency.
#' Only the type `absolute` of (\code{\link[processmapR]{precedence_matrix}}) is supported.
#'
#' @param eventlog The event log object to be used.
#' @param lead The distance between activities following/preceding each other.
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(traffic_fines)
#' m <- precedence_matrix_absolute(traffic_fines)
#' print(m)
#' as.matrix(m)
#' }
#'
#' @export precedence_matrix_absolute
precedence_matrix_absolute <- function(eventlog, lead = 1) {
  stopifnot("eventlog" %in% class(eventlog))
  stopifnot(lead > 0)

  eventlog <- reduce_simple_eventlog(eventlog)
  precedence_matrix_absolute_impl(eventlog, lead)
}

precedence_matrix_absolute_impl <- function(simplelog, lead = 1) {
  mat <- as_tibble(count_precedence(simplelog$case_id,
                                    simplelog$activity_id,
                                    lead))

  class(mat) <- c("precedence-matrix", class(mat))
  attr(mat, "matrix_type") <- "absolute"
  return(mat)
}
