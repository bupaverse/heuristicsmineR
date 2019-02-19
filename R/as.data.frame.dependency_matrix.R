#' @export
as.data.frame.dependency_matrix <- function(x, row.names=NULL, optional=FALSE, ...) {
  df <- as.data.frame.table(x, responseName = "dep")
  df[df$dep > 0,]
}
