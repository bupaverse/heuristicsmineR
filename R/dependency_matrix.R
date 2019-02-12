#' Create a dependency matrix
#'
#' Creates a dependency matrix from a precedence matrix (\code{\link{precedence_matrix}}) based on different approaches.
#'
#' @param eventlog A bupaR event log, may be NULL when a precedence matrix is provided.
#' @param precedence A precedence matrix as data frame, for example, created from a bupaR event log using (\code{\link{precedence_matrix_absolute}}).
#' @param threshold A dependency threshold in the interval `[0,1]` filtering out dependencies below the threshold.
#' @param type Which approach to use for calculation of the dependency matrix. Currently only `FHM` available.
#'
#' @return A data frame with class `dependency_matrix` containing the computed dependency values between activities.
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
                              precedence = precedence_matrix_absolute(eventlog),
                              threshold = 0.9,
                              type = c("FHM")) {

  . <- act <- antecedent <- consequent <- dep <- a_to_c  <- c_to_a <- n.x <- n.y <- na.omit <- NULL

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
    rename(a_to_c = n.x,
           c_to_a = n.y) %>%
    # dependencies
    mutate(dep = if_else(antecedent != consequent, (a_to_c - c_to_a) / (a_to_c + c_to_a + 1), NA_real_)) %>%
    # l1 loops
    mutate(dep = if_else(antecedent == consequent, (a_to_c / (a_to_c + 1)), dep)) %>%
    na.omit() %>%
    filter(dep >= threshold)

  class(dependencies) <- c("dependency_matrix", class(dependencies))

  dependencies
}
