#' @export
as.matrix.dependency_matrix <- function(x, rownames=NULL, rownames.value=NULL, ...) {

  labels <- sort(unique(c(as.character(x$antecedent), as.character(x$consequent))))
  x$antecedent <- factor(x$antecedent, labels)
  x$consequent <- factor(x$consequent, labels)

  m <- stats::xtabs(formula = dep ~ antecedent + consequent,
               data = x,
               subset = NULL)

  m <- matrix(m, nrow = length(labels), ncol = length(labels))
  colnames(m) <- labels
  rownames(m) <- labels
  names(dimnames(m)) <- c("antecedent", "consequent")

  m
}

#' @export
`as.matrix.precedence-matrix` <- function(x, rownames=NULL, rownames.value=NULL, ...) {

  labels <- sort(unique(c(as.character(x$antecedent), as.character(x$consequent))))
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
