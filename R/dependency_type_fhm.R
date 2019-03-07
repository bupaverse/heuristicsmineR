
#' Flexible heuristics miner dependency type
#'
#' @param threshold A dependency threshold, usually in the interval `[0,1]`, filtering out dependencies below the threshold.
#' @param threshold_l1 A dependency threshold, usually in the interval `[0,1]`, filtering out self-loop dependencies below the threshold.
#' @param threshold_l2 A dependency threshold, usually in the interval `[0,1]`, filtering out length-two loop dependencies below the threshold.
#' @param all_connected If `TRUE` the best antecedent and consequent (as determined by the dependency measure) are going to be added regardless of the `threshold` value.
#' @param precedence A precedence matrix overriding the default matrix generated from the event log. For example, created from a bupaR event log using (\code{\link{precedence_matrix_absolute}}).
#' @param precedence_lenght_two_loops A precedence matrix of length two loop patterns (aba) overriding the default matrix generated from the event log. For example, created from a bupaR event log using (\code{\link{precedence_matrix_length_two_loops}}).
#'
#' @return A dependency type.
#' @export
#'
#' @examples
#' dependency_matrix(L_heur_1,
#'                   type = dependency_type_fhm(all_connected = TRUE))
#'
dependency_type_fhm <- function(threshold = 0.9,
                                threshold_l1 = threshold,
                                threshold_l2 = threshold,
                                all_connected = FALSE,
                                precedence = NULL,
                                precedence_lenght_two_loops = NULL) {

  dependency_type <- "fhm"
  class(dependency_type) <- c("dependency_type", class(dependency_type))
  attr(dependency_type, "name") <- "Flexible Heuristics Miner"
  attr(dependency_type, "compute") <- function(eventlog) {

    . <- act <- antecedent <- consequent <- dep <- a_to_c  <- c_to_a <- n.x <- n.y <- na.omit <- NULL

    if (is.null(precedence)) {
      # Compute precedence only sorting log once
      simplelog <- reduce_simple_eventlog(eventlog)
      precedence <- precedence_matrix_absolute_impl(simplelog)
      if (is.null(precedence_lenght_two_loops)) {
        precedence_lenght_two_loops <- precedence_matrix_length_two_loops_impl(simplelog)
      }
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

    # Filter by threshold
    mat[mat < threshold_l2] <- 0.0

    # L1 loops
    diag(mat) <- diag(mat_pre) / (diag(mat_pre) + 1)

    # Filter by threshold
    mat[mat < threshold_l1] <- 0.0

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

    # All connected heuristic
    if (all_connected) {
      # Add best antecedents (rows)
      missing_rows <- rowSums(mat) == 0
      row_zero <- dep_mat[missing_rows, , drop=FALSE] # no consequents
      row_max <- apply(row_zero, 1, max)
      # keep only best
      row_zero[row_zero < row_max] <- 0
      mat[missing_rows,] <- row_zero

      # Add best consequents (columns)
      missing_cols <- colSums(mat) == 0
      col_zero <- dep_mat[, missing_cols, drop=FALSE] # no consequents
      col_max <- apply(col_zero, 2, max)
      # keep only best
      col_zero[col_zero < rep(col_max, each = nrow(col_zero))] <- 0
      mat[, missing_cols] <- col_zero
    }

    mat
  }

  dependency_type
}
