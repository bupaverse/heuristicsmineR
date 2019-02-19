#' Create a dependency matrix
#'
#' Creates a dependency matrix from a precedence matrix (\code{\link{precedence_matrix}}) based on different approaches.
#'
#' @param eventlog A bupaR event log, may be NULL when a precedence matrix is provided.
#' @param precedence A precedence matrix as data frame, for example, created from a bupaR event log using (\code{\link{precedence_matrix_absolute}}).
#' @param precedence_lenght_two_loops A precedence matrix of length two loop patterns (aba) as data frame, for example, created from a bupaR event log using (\code{\link{precedence_matrix_length_two_loops}}).
#' @param threshold A dependency threshold, usually in the interval `[0,1]`, filtering out dependencies below the threshold.
#' @param type Which approach to use for calculation of the dependency matrix. Currently only `FHM` available.
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
                              precedence = NULL,
                              precedence_lenght_two_loops = NULL,
                              threshold = 0.9,
                              type = c("FHM")) {

  . <- act <- antecedent <- consequent <- dep <- a_to_c  <- c_to_a <- n.x <- n.y <- na.omit <- NULL

  if (!is.null(eventlog)) {
    # Compute precedence only sorting log once
    simplelog <- reduce_simple_eventlog(eventlog)
    precedence <- precedence_matrix_absolute_impl(simplelog)
    precedence_lenght_two_loops <- precedence_matrix_length_two_loops_impl(simplelog)
  }

  if (!all(c("antecedent","consequent", "n") %in% names(precedence))) {
    stop("Input `precedence` needs to be a data frame with column `antecedent`, `consequent`, and `n`!")
  }

  # Prepare precedence matrix
  mat_pre <-`as.matrix.precedence-matrix`(precedence)
  acts <- colnames(mat_pre)
  t_mat_pre <- t(mat_pre)

  # L2 loops
  if (!is.null(precedence_lenght_two_loops)) {
    mat_loops <-`as.matrix.precedence-matrix`(precedence_lenght_two_loops)
    t_mat_loops <- t(mat_loops)
    mat <- (mat_loops - t_mat_loops) / (mat_loops + t_mat_loops + 1)
  }

  # L1 loops
  diag(mat) <- diag(mat_pre) / (diag(mat_pre) + 1)

  # Filter by threshold
  mat[mat < threshold] <- 0.0

  # Filter out L2 loops already in L1 loops above threshold
  l1_vals <- diag(mat)
  l1_dep <- l1_vals[l1_vals > 0]

  filtered_cols <- which(colnames(mat) %in% names(l1_dep))
  filtered_rows <- which(rownames(mat) %in% names(l1_dep))

  mat[filtered_rows, ] <- 0.0
  mat[,filtered_cols] <- 0.0
  # Restore L1 values
  diag(mat) <- l1_vals

  # Standard dependencies
  dep_mat <- (mat_pre - t_mat_pre) / (mat_pre + t_mat_pre + 1)

  # Add those better than existing
  mat[dep_mat > mat] <- dep_mat[which(dep_mat > mat, arr.ind = TRUE)]
  mat[mat < threshold] <- 0.0

  #TODO Long distance dependencies, All connected / Accepted connected

  class(mat) <- c("dependency_matrix", class(mat))

  mat
}
