#' Dependency type based on time intervals
#'
#' Computes the dependencies based on the approach taking into account activity durations based on life-cycle transitions.
#'
#' @param threshold_dependency A dependency threshold, usually in the interval `[0,1]`, filtering out dependencies below the threshold.
#' @param threshold_l1 A dependency threshold, usually in the interval `[0,1]`, filtering out self-loop dependencies below the threshold.
#' @param threshold_frequency An absolute frequency threshold filtering dependencies which are observed infrequently.
#' @param all_connected If `TRUE` the best antecedent and consequent (as determined by the dependency measure) are going to be added regardless of the `threshold` value.
#'
#' @return A dependency type.
#' @export
#' @references A. Burattin and A. Sperduti, “Heuristics Miner for Time Intervals,” in ESANN 2010, 18th European Symposium on Artificial Neural Networks, Bruges, Belgium, April 28-30, 2010, Proceedings, 2010.
#'
#' @examples
#' dependency_matrix(L_heur_1,
#'                   dependency_type = dependency_type_fhm(all_connected = TRUE))
#'
dependency_type_lifecycle <- function(threshold_dependency = 0.9,
                                      threshold_l1 = threshold_dependency,
                                      threshold_frequency = 0,
                                      all_connected = FALSE) {

  dependency_type <- "fhm"
  class(dependency_type) <- c("dependency_type", class(dependency_type))
  attr(dependency_type, "name") <- "Time Intervals"

  attr(dependency_type, "compute") <- function(eventlog) {

    . <- act <- antecedent <- consequent <- dep <- a_to_c  <- c_to_a <- n.x <- n.y <- na.omit <- NULL

    # Compute precedence only sorting log once
    simplelog <- reduce_activitylog(eventlog)
    precedence <- precedence_matrix_lifecycle_impl(simplelog)
    parallel <- parallel_matrix_lifecycle_impl(simplelog)

    if (!all(c("antecedent","consequent", "n") %in% names(precedence))) {
      stop("Input `precedence` needs to be a data frame with column `antecedent`, `consequent`, and `n`!")
    }

    # Prepare precedence matrix
    mat_pre <- as.matrix(precedence)
    acts <- colnames(mat_pre)
    t_mat_pre <- t(mat_pre)

    # Prepare parallel matrix
    mat_par <- as.matrix(parallel)

    # TODO how to deal with L2 loops?
    mat <- matrix(0, nrow = length(acts), ncol = length(acts))
    colnames(mat) <- acts
    rownames(mat) <- acts
    names(dimnames(mat)) <- c("antecedent", "consequent")

    # L1 loops
    # TODO can this be left unchanged?
    diag(mat) <- diag(mat_pre) / (diag(mat_pre) + 1)

    # Filter by l1 threshold
    mat[mat < threshold_l1] <- 0.0
    diag(mat)[diag(mat_pre) < threshold_frequency] <- 0.0

    # Standard dependencies
    dep_mat <- (mat_pre - t_mat_pre) / (mat_pre + t_mat_pre + 2 * mat_par + 1)

    # Add those better than existing
    mat[dep_mat > mat] <- dep_mat[which(dep_mat > mat, arr.ind = TRUE)]
    mat[mat < threshold_dependency | mat_pre < threshold_frequency] <- 0.0

    # All connected heuristic
    if (all_connected) {

      # Add best consequents (rows)
      missing_rows <- (rowSums(mat) - diag(mat)) == 0  # subtract diag(mat) to not count self loops
      row_zero <- dep_mat[missing_rows, , drop=FALSE] # no consequents
      row_max <- apply(row_zero, 1, max)
      # keep only best
      row_zero[row_zero < row_max] <- 0
      mat[missing_rows,] <- row_zero

      # Add best antecedents (columns)
      missing_cols <- (colSums(mat) - diag(mat)) == 0 # subtract diag(mat) to not count self loops
      col_zero <- dep_mat[, missing_cols, drop=FALSE] # no antecedents
      col_max <- apply(col_zero, 2, max)
      # keep only best
      col_zero[col_zero < rep(col_max, each = nrow(col_zero))] <- 0
      mat[, missing_cols] <- col_zero

      # For those without any consequent add artifical END dependency
      missing_rows <- (rowSums(mat) - diag(mat)) == 0  # subtract diag(mat) to not count self loops
      row_zero <- dep_mat[missing_rows, , drop=FALSE] # no consequents
      row_zero[row_zero < 0] <- 0
      row_zero[rownames(row_zero) != "End", colnames(row_zero) == "End"] <- 1
      row_zero[rownames(row_zero) == "End",] <- 0
      mat[missing_rows,] <- row_zero

      # For those without any antecedent add artificial START dependency
      missing_cols <- (colSums(mat) - diag(mat)) == 0 # subtract diag(mat) to not count self loops
      col_zero <- dep_mat[, missing_cols, drop=FALSE] # no antecedents
      col_zero[col_zero < 0] <- 0
      col_zero[rownames(col_zero) == "Start", colnames(col_zero) != "Start"] <- 1
      col_zero[,colnames(col_zero) == "Start"] <- 0
      mat[, missing_cols] <- col_zero
    }

    mat
  }

  dependency_type
}
