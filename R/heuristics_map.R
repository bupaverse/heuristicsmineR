#' Create a dependency graph
#'
#' Creates a dependency graph visualizing a dependency matrix and
#'
#' @param dependencies A dependency matrix created by \code{\link{dependency_matrix}}
#' @param rankdir Rankdir to be used for DiagrammeR.
#' @param layout Layout to be used for DiagrammeR.
#' @param render Whether to directly render the DiagrammeR graph or simply return it.
#'
#' @return A DiagrammeR graph of the (filtered) dependency matrix.
#'
#' @examples
#' library(eventdataR)
#' library(processmapR)
#' data(patients)
#' dependency_graph(dependency_matrix(patients))
#'
#' @import dplyr
#' @import DiagrammeR
#' @export
dependency_graph <- function(dependencies,
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

	activities <- union(dependencies$antecedent,
	                    dependencies$consequent)

  base_nodes <- data.frame(act = activities, stringsAsFactors = FALSE) %>%
    mutate(id = 1:n())

  nodes <- base_nodes %>%
			mutate(shape = if_end(act, "circle", "rectangle"),
				   fontcolor = if_end(act, if_start(act, "chartreuse4","brown4"), "black"),
				   color = if_end(act, if_start(act, "chartreuse4","brown4"), "grey"),
				   label = act)

  create_node_df(n = nrow(nodes),
				   label = nodes$label,
				   shape = nodes$shape,
				   style = "rounded,filled",
				   fontcolor = nodes$fontcolor,
				   color = nodes$color,
				   penwidth = 1.5,
				   fixedsize = FALSE) -> nodes_df

  edges_df <- dependencies %>%
    				left_join(base_nodes, by = c("antecedent" = "act")) %>%
					 	rename(from_id = id) %>%
    			  left_join(base_nodes, by = c("consequent" = "act")) %>%
					 	rename(to_id = id)

	create_edge_df(from = edges_df$from_id,
				   to = edges_df$to_id,
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

#' Create a causal map
#'
#' Creates a causal map which is similar to a processmapR process map. However, the causal map
#' deals with parallelism by trying to identifying causal dependencies between activities using
#' several heuristics.
#'
#' Warning: Projected frequencies are WORK IN PROGRESS and currently not reliable!
#'
#' @param processmap A proces map created by processmapR.
#' @param dependencies A dependency matrix created for the event log, for example, by \code{\link{dependency_matrix}}.
#' @param rankdir Rankdir to be used for DiagrammeR.
#' @param layout Layout to be used for DiagrammeR.
#' @param render Whether to directly render the DiagrammeR graph or simply return it.
#'
#' @return A DiagrammeR graph of the causal map.
#'
#' @examples
#' library(eventdataR)
#' library(processmapR)
#' data(patients)
#' causal_map(patients, dependency_matrix(patients))
#'
#' @import dplyr
#' @import DiagrammeR
#' @export
causal_map <- function(processmap,
                       dependencies,
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

	# Create causal graph

	activities <- union(dependencies$antecedent,
	                    dependencies$consequent)

  base_nodes <- data.frame(act = activities, stringsAsFactors = FALSE) %>%
    mutate(id = 1:n())

  nodes <- base_nodes %>%
			mutate(shape = if_end(act, "circle", "rectangle"),
				   fontcolor = if_end(act, if_start(act, "chartreuse4","brown4"), "black"),
				   color = if_end(act, if_start(act, "chartreuse4","brown4"), "grey"),
				   label = act)

  edges <- dependencies %>%
    				left_join(base_nodes, by = c("antecedent" = "act")) %>%
					 	rename(from_id = id) %>%
    			  left_join(base_nodes, by = c("consequent" = "act")) %>%
					 	rename(to_id = id)

  # Derive precedence data frame that mirrors the one from processmapR

  base_precedence <- attr(processmap, "base_precedence")

  # Create DiagrammeR graph

  create_node_df(n = nrow(nodes),
				   label = nodes$label,
				   shape = nodes$shape,
				   style = "rounded,filled",
				   fontcolor = nodes$fontcolor,
				   color = nodes$color,
				   penwidth = 1.5,
				   fixedsize = FALSE) -> nodes_df

	create_edge_df(from = edges$from_id,
				   to = edges$to_id,
				   label =  round(edges$n.x, 2)) -> edges_df

	create_graph(nodes_df, edges_df) %>%
		add_global_graph_attrs(attr = "rankdir", value = rankdir, attr_type = "graph") %>%
		add_global_graph_attrs(attr = "layout", value = layout, attr_type = "graph") -> graph

	if(render == T) {
		graph %>% render_graph() -> graph
	  attr(graph, "base_precedence") <- base_precedence
		graph %>% return()
	} else  {
	  attr(graph, "base_precedence") <- base_precedence
		graph %>% return()
	}

}

