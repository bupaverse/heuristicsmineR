#' Renders a dependency matrix as dependency graph
#'
#' Creates a dependency graph visualizing the contents of a dependency matrix.
#'
#' @param dependencies A dependency matrix created by \code{\link{dependency_matrix}}
#' @param rankdir Rankdir to be used for DiagrammeR.
#' @param layout Layout to be used for DiagrammeR.
#' @param render Whether to directly render the DiagrammeR graph or simply return it.
#'
#' @return A DiagrammeR graph of the (filtered) dependency matrix.
#'
#' @examples
#' render_dependency_matrix(dependency_matrix(L_heur_1))
#'
#' @import dplyr
#' @import DiagrammeR
#' @export
render_dependency_matrix <- function(dependencies,
                                     rankdir = "LR",
                                     layout = "dot",
                                     render = T) {

  act <- dep <- NULL

	if_end <- function(node, true, false) {
		ifelse(node %in% c("Start","End"), true, false)
	}
	if_start <- function(node, true, false) {
		ifelse(node %in% c("Start"), true, false)
	}

	activities <- colnames(dependencies)
  dependencies <- as.data.frame(dependencies)

  base_nodes <- data.frame(act = activities, stringsAsFactors = FALSE) %>%
    mutate(id = 1:n())

  nodes <- base_nodes %>%
			mutate(shape = if_end(act, "circle", "rectangle"),
				   fontcolor = if_end(act, if_start(act, "chartreuse4","brown4"), "black"),
				   color = if_end(act, if_start(act, "chartreuse4","brown4"), "black"),
				   label = act)

  create_node_df(n = nrow(nodes),
				   label = nodes$label,
				   shape = nodes$shape,
				   style = "",
				   fontcolor = nodes$fontcolor,
				   color = nodes$color,
				   penwidth = 1.5,
				   fixedsize = FALSE) -> nodes_df

  suppressWarnings( # factor / char warning
    edges_df <- dependencies %>%
      				left_join(base_nodes, by = c("antecedent" = "act")) %>%
  					 	rename(from_id = id) %>%
      			  left_join(base_nodes, by = c("consequent" = "act")) %>%
  					 	rename(to_id = id)
  )

	create_edge_df(from = edges_df$from_id,
				   to = edges_df$to_id,
				   color = "black",
				   label =  round(edges_df$dep, 2)) -> edges_df

	create_graph(nodes_df, edges_df) %>%
		add_global_graph_attrs(attr = "rankdir", value = rankdir, attr_type = "graph") %>%
		add_global_graph_attrs(attr = "layout", value = layout, attr_type = "graph") -> graph

	if(render == T) {
		graph %>% render_graph() -> graph
		graph %>% return()
	} else  {
		graph %>% return()
	}

}
