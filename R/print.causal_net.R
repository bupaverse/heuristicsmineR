#' Generic print function for a Causal net
#'
#' @param x Causal net object
#' @param ... Additional Arguments
#' @export
print.causal_net <- function(x, ...) {
  cat("Nodes\n")
  print(x$nodes)
  cat("Edges\n")
  print(x$edges)
  print(render_causal_net(x))
}
