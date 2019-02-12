#' Precendence Matrix
#'
#' Construct a precendence matrix, showing how activities are followed by each other. This function computes the precedence matrix directly in C++ for efficiency.
#'  Only the type `absolute` of (\code{\link[processmapR]{precedence_matrix}}) is supported.
#'
#' @param eventlog The event log object to be used
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
precedence_matrix_absolute <- function(eventlog) {
  stopifnot("eventlog" %in% class(eventlog))

  antecedent <- NULL
  consequent <- NULL
  ts <- NULL
  min_order <- NULL
  .order <- NULL

  log <- eventlog %>%
    group_by(
      !!case_id_(eventlog),
      !!activity_id_(eventlog),
      !!activity_instance_id_(eventlog)
    ) %>%
    summarize(ts = min(!!timestamp_(eventlog)),
              min_order = min(.order))  %>%
    arrange(!!case_id_(eventlog), ts, min_order) %>%
    select(!!case_id_(eventlog), !!activity_id_(eventlog))

  mat <- as_tibble(count_precedence(log[[case_id(eventlog)]], log[[activity_id(eventlog)]]))
  class(mat) <- c("precedence-matrix", class(mat))
  attr(mat, "matrix_type") <- "absolute"
  return(mat)
}
