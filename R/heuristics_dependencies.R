#' Create a dependency matrix
#'
#' Creates a dependency matrix from a precedence matrix (\code{\link{precedence_matrix}}) based on different approaches.
#'
#' @param eventlog A bupaR event log, may be NULL when a precedence matrix is provided.
#' @param precedence A precedence matrix, for example, created from a bupaR event log using (\code{\link{precedence_matrix}}).
#' @param threshold A dependency threshold in the interval `[0,1]` filtering out dependencies below the threshold.
#' @param type Which approach to use for calculation of the dependency matrix. Currently only `FHM` available.
#'
#' @return A data frame with the dependency values between activities.
#'
#' @seealso \code{\link{precedence_matrix}}, \code{\link{dependency_graph}}
#'
#' @examples
#' library(eventdataR)
#' library(processmapR)
#' data(patients)
#' dependency_matrix(patients)
#'
#' @import dplyr
#' @import bupaR
#' @import processmapR
#' @importFrom stats na.omit
#'
#' @export
dependency_matrix <- function(eventlog = NULL,
                              precedence = precedence_matrix(eventlog, type = "absolute"),
                              threshold = 0.0,
                              type = c("FHM")) {

   . <- act <- antecedent <- consequent <- dep <- n.x <- n.y <- na.omit <- NULL

  if (!all(c("antecedent","consequent", "n") %in% names(precedence))) {
    stop("Input `precedence` needs to be a data frame with column `antecedent`, `consequent`, and `n`!")
  }

  precedence <- precedence %>%
    mutate(antecedent = as.character(antecedent),
           consequent = as.character(consequent))

  # FHM dependency measure
  dependencies <- precedence %>%
    left_join(precedence,
              by = c("antecedent" = "consequent",
                     "consequent" = "antecedent")) %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>%
    # dependencies
    mutate(dep = if_else(antecedent != consequent, (n.x - n.y) / (n.x + n.y + 1), NA_real_)) %>%
    # l1 loops
    mutate(dep = if_else(antecedent == consequent, (n.x / (n.x + 1)), dep)) %>%
    na.omit() %>%
    filter(dep >= threshold)

  class(dependencies) <- c("dependency_matrix", class(dependencies))

  dependencies
}
