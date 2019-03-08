#' Dependency matrix plot
#'
#' Visualize a dependency matrix. A generic plot function for dependency matrices.
#'
#' @param x Dependency matrix
#' @param ... Additional paramters
#' @return A ggplot object, which can be customized further, if deemed necessary.
#' @export
#' @import ggplot2
plot.dependency_matrix <- function(x, ...) {
	antecedent <- NULL
	consequent <- NULL

	x <- as.data.frame(x)

	x %>%
		ggplot(aes(antecedent, consequent)) +
		geom_raster(aes(fill = dep)) +
		geom_text(aes(label = round(dep*100, 2)), color = "white", fontface = "bold") +
		ggthemes::scale_fill_continuous_tableau(name = "Dependency measure", limits = c(0,1)) +
		theme_light() +
		coord_flip() +
		theme(axis.text.x = element_text(angle = 45, hjust = 1))-> p
	p <- p + labs(x = "Antecedent", y = "Consequent")

	p
}
