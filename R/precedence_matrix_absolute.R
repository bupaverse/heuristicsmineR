#' Precedence Matrix
#'
#' Construct a precedence matrix, showing how activities are followed by each other.
#' This is a performance improved variant of \code{\link[processmapR]{precedence_matrix}}
#' in the processmapR package.
#'
#' @inheritParams processmapR::precedence_matrix
#'
#' @export
#'
#' @examples
#' m <- precedence_matrix(hospital_multi_perspective, type = "absolute")
#' print(m)
#' as.matrix(m)
#'
precedence_matrix <- function(eventlog, type = c("absolute","relative","relative-antecedent","relative-consequent", "relative-case")) {

  antecedent <- consequent <- NULL

	stopifnot("eventlog" %in% class(eventlog))

  m <- precedence_matrix_absolute(eventlog)

  if (type == "absolute") {
    # nothing
  } else if (type == "relative") {
    m %>%
      mutate(rel_n = n / sum(n)) -> m

  }
  else if (type == "relative-antecedent") {
    m %>%
      group_by(antecedent) %>%
      mutate(rel_antecedent = n / sum(n)) %>%
      ungroup() -> m

  }
  else if (type == "relative-consequent") {
    m %>%
      group_by(consequent) %>%
      mutate(rel_consequent = n / sum(n)) %>%
      ungroup() -> m

  } else {
    m <- processmapR::precedence_matrix(eventlog, type)
  }

  if (!("process_matrix" %in% class(m))) {
    class(m) <- c("process_matrix", class(m))
  }
  attr(type, "perspective") <- "frequency"
  attr(m, "matrix_type") <- type

  m
}

#' Precedence Matrix
#'
#' Construct a precedence matrix, showing how activities are followed by each other.
#' This function computes the precedence matrix directly in C++ for efficiency.
#' Only the type `absolute` of (\code{\link[processmapR]{precedence_matrix}}) is supported.
#'
#' @param eventlog The event log object to be used.
#' @param lead The distance between activities following/preceding each other.
#'
#' @examples
#' library(eventdataR)
#' data(traffic_fines)
#' m <- precedence_matrix_absolute(traffic_fines)
#' print(m)
#' as.matrix(m)
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

  class(mat) <- c("process_matrix", class(mat))
  type <- "absolute"
  attr(type, "perspective") <- "frequency"
  attr(mat, "matrix_type") <- type
  return(mat)
}
