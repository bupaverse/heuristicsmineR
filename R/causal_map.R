#
# Partly based on code from https://github.com/gertjanssenswillen/processmapR
# (c) Hasselt University released under MIT license
#

#' Create a causal map
#'
#' Creates a causal map which is similar to a processmapR process map. However, the causal map
#' deals with parallelism by trying to identifying causal dependencies between activities using
#' several heuristics.
#'
#' Warning: Projected frequencies are heuristically determined and counts may not add up.
#'
#' @param eventlog The event log for which a causal map should be computed.
#'  Can be left NULL for more control if parameters `dependencies` and `bindings` are provided directly.
#' @param dependencies A dependency matrix created for the event log, for example, by \code{\link{dependency_matrix}}.
#' @param threshold The dependency threshold to be used when using the default dependency matrix computation.
#' @param bindings Causal bindings created by \code{\link{causal_bindings}}.
#' @param type A causal map type. Currently only function \code{\link{causal_frequency}} is supported.
#' @param type_edges A causal map type to be used for edges only.
#'  Currently only function \code{\link{causal_frequency}} is supported.
#' @param type_nodes A causal map type to be used for nodes only.
#'  Currently only function \code{\link{causal_frequency}} is supported.
#' @param rankdir Rankdir to be used for DiagrammeR.
#' @param layout Layout to be used for DiagrammeR.
#' @param render Whether to directly render the DiagrammeR graph or simply return it.
#' @param fixed_edge_width If TRUE, don't vary the width of edges.
#' @param fixed_node_pos When specified as a data.frame with three columns 'act', 'x', and 'y' the position of nodes is fixed. Note that his can only be used with the 'neato' layout engine.
#' @param ... Further parameters forwarded to the DiagrammeR render function.
#'
#' @return A DiagrammeR graph of the causal map.
#'
#' @examples
#' # Causal map with default parameters
#' causal_map(L_heur_1)
#'
#' # Causal map with lower dependency treshold
#' causal_map(L_heur_1, threshold = .8)
#'
#' # For even more control omit the `eventlog` parameter
#' # and provide `dependencies` and `bindings` directly.
#' d <- dependency_matrix(L_heur_1, threshold = .8)
#' causal_map(dependencies = d,
#'            bindings = causal_bindings(L_heur_1, d, "nearest"))
#'
#' @import dplyr
#' @import DiagrammeR
#' @export
causal_map <- function(eventlog = NULL,
                       dependencies = dependency_matrix(eventlog, threshold = threshold),
                       threshold = 0.9,
                       bindings = causal_bindings(eventlog, dependencies),
                       type = causal_frequency("absolute"),
      								 type_nodes = type,
      								 type_edges = type,
                       rankdir = "LR",
                       layout = "dot",
                       render = T,
      								 fixed_edge_width = F,
      								 fixed_node_pos = NULL,
      								 ...) {

  act <- dep <- binding <- binding_input <- binding_output <- bindings_input <-
    bindings_output <- case <- color_level <- from_id <- label <- NULL

	extra_data <- list()
	extra_data$n_cases <- length(unique(bindings$case))
	extra_data$n_activity_instances <- length(unique(bindings$aid))

	nodes <- attr(type_nodes, "create_nodes")(bindings, type_nodes, extra_data)
	edges <- attr(type_edges, "create_edges")(dependencies, bindings, type_edges, extra_data)

	if(fixed_edge_width) {
		edges %>% mutate(penwidth = 1) -> edges
	}

	nodes %>%
		mutate(color_level = scales::rescale(color_level, from = c(0, max(color_level)))) %>%
		mutate(color_level = if_end(act, Inf, color_level)) %>%
    arrange(from_id) -> nodes # `create_node_df` needs nodes in order of identifier

	create_node_df(n = nrow(nodes),
				   label = nodes$label,
				   shape = nodes$shape,
				   color_level = nodes$color_level,
				   style = "filled",
				   fontcolor = nodes$fontcolor,
				   color = nodes$color,
				   tooltip = nodes$tooltip,
				   penwidth = 1.5,
				   fixedsize = FALSE,
				   fontname = "Arial",
				   fontsize = 10) -> nodes_df

	if (is.data.frame(fixed_node_pos)) {
		nodes %>%
			left_join(fixed_node_pos, by = c("ACTIVITY_CLASSIFIER_" = "act")) -> nodes
		nodes_df %>% mutate(x = nodes$x, y = nodes$y) -> nodes_df
	}

	min_level <- min(nodes_df$color_level)
	max_level <- max(nodes_df$color_level[nodes_df$color_level < Inf])

	create_edge_df(from = edges$from_id,
				   to = edges$to_id,
				   label = edges$label,
				   penwidth = edges$penwidth,
				   color = attr(type_edges, "color_edges"),
				   fontname = "Arial",
				   fontsize = 10) -> edges_df

	create_graph(nodes_df, edges_df) %>%
		add_global_graph_attrs(attr = "rankdir", value = rankdir,attr_type = "graph") %>%
		add_global_graph_attrs(attr = "layout", value = if_else(is.data.frame(fixed_node_pos), "neato", "dot"), attr_type = "graph") %>%
		colorize_node_attrs(node_attr_from = "color_level",
							node_attr_to = "fillcolor",
							palette = attr(type_nodes, "color"),
							default_color = "white",
							cut_points = seq(min_level-0.1, max_level+.1, length.out = 9)) -> graph

	if(render == T) {
		graph %>% render_graph(...) -> graph
	  attr(graph, "dependencies") <- dependencies
	  attr(graph, "bindings") <- bindings
		graph %>% return()
	} else  {
	  attr(graph, "dependencies") <- dependencies
	  attr(graph, "bindings") <- bindings
		graph %>% return()
	}

}
