#' Generic print function for a dependency matrix
#'
#' @param x dependency matrix object
#' @param ... Additional Arguments
#' @export
print.dependency_matrix <- function(x, ...) {
  NextMethod("print", x)
  print(render_dependency_matrix(x))
}
