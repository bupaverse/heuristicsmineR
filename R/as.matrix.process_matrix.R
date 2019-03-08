#' @export
as.matrix.process_matrix <- function(x, rownames=NULL, rownames.value=NULL, ...) {

  if (is.factor(x$antecedent) && is.factor(x$consequent)) {
    # our own precedence matrix returns factors
    labels <- sort(union(levels(x$antecedent),levels(x$consequent)))
  } else {
    labels <- sort(unique(c(as.character(x$antecedent), as.character(x$consequent))))
  }

  x$antecedent <- factor(x$antecedent, labels)
  x$consequent <- factor(x$consequent, labels)

  m <- stats::xtabs(formula = n ~ antecedent + consequent,
               data = x,
               subset = NULL)

  m <- matrix(m, nrow = length(labels), ncol = length(labels))
  colnames(m) <- labels
  rownames(m) <- labels
  names(dimnames(m)) <- c("antecedent", "consequent")

  m
}
