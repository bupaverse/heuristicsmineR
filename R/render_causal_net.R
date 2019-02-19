#' Renders a Causal net as graph
#'
#' @param causal_net A causal net created by \code{\link{causal_net}}
#' @param rankdir Rankdir to be used for DiagrammeR.
#' @param layout Layout to be used for DiagrammeR.
#' @param render Whether to directly render the DiagrammeR graph or simply return it.
#' @param fixed_edge_width If TRUE, don't vary the width of edges.
#' @param fixed_node_pos When specified as a data.frame with three columns 'act', 'x', and 'y' the position of nodes is fixed. Note that his can only be used with the 'neato' layout engine.
#' @param ... Further parameters forwarded to the DiagrammeR render function.
#'
#' @return A DiagrammeR graph of the Causal net.
#'
#' @examples
#' render_causal_net(causal_net(L_heur_1))
#'
#' @import DiagrammeR
#' @export
render_causal_net <- function(causal_net,
                              rankdir = "LR",
                              layout = "dot",
                              render = T,
                              fixed_edge_width = F,
                              fixed_node_pos = NULL,
                              ...) {

  color_level <- act <- from_id <- NULL

  nodes <- causal_net$nodes
  edges <- causal_net$edges
  type_edges <- attr(causal_net, "type_edges")
  type_nodes <- attr(causal_net, "type_nodes")

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
		graph %>% return()
	} else  {
		graph %>% return()
	}

}
