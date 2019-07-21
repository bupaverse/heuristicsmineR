#' Create a Causal net (also Heuristics net)
#'
#' Creates a Causal net, also known as Heuristics net. This is similar to a processmapR process map.
#' However, the causal map deals with parallelism by trying to identifying causal dependencies
#' between activities by using different heuristics as documented in \code{\link{dependency_matrix}}.
#'
#' Warning: Projected frequencies are heuristically determined and counts may not add up.
#'
#' @param eventlog The event log for which a causal map should be computed.
#'  Can be left NULL for more control if parameters `dependencies` and `bindings` are provided directly.
#' @param dependencies A dependency matrix created for the event log, for example, by \code{\link{dependency_matrix}}.
#' @param threshold The dependency threshold to be used when using the default dependency matrix computation.
#' @param threshold_frequency The frequency threshold to be used when using the default dependency matrix computation.
#' @param bindings Causal bindings created by \code{\link{causal_bindings}}.
#' @param type A causal map type. For example, \code{\link{causal_frequency}} or \code{\link{causal_performance}}.
#' @param sec A causal process map type. Values are shown between brackets.
#' @param type_edges A causal map type to be used for edges only.
#' @param type_nodes A causal map type to be used for nodes only.
#' @param sec_nodes A secondary causal map type for nodes only.
#' @param sec_edges A secondary causal map type for edges only.
#' @param ... Further parameters forwarded to the default \code{\link{dependency_matrix}} function.
#'
#' @return A DiagrammeR graph of the causal map.
#'
#' @examples
#' # Causal map with default parameters
#' causal_net(L_heur_1)
#'
#' # Causal map with lower dependency treshold
#' causal_net(L_heur_1, threshold = .8)
#'
#' # For even more control omit the `eventlog` parameter
#' # and provide `dependencies` and `bindings` directly.
#' d <- dependency_matrix(L_heur_1, threshold = .8)
#' causal_net(dependencies = d,
#'            bindings = causal_bindings(L_heur_1, d, "nearest"))
#' \donttest{
#' # The returned DiagrammeR object can be further augmented with
#' # panning and zooming before rendering:
#' library(magrittr)
#' causal_net(L_heur_1) %>%
#'  render_causal_net(render = T) %>%
#'  DiagrammeRsvg::export_svg() %>%
#'  svgPanZoom::svgPanZoom()
#' }
#'
#' @import dplyr
#' @import DiagrammeR
#' @export
causal_net <- function(eventlog = NULL,
                       dependencies = dependency_matrix(eventlog = eventlog,
                                                        threshold = threshold,
                                                        threshold_frequency = threshold_frequency,
                                                        ...),
                       bindings = causal_bindings(eventlog,
                                                  dependencies),
                       threshold = 0.9,
                       threshold_frequency = 0,
                       type = causal_frequency("absolute"),
      								 sec = NULL,
                       type_nodes = type,
      								 type_edges = type,
      								 sec_nodes = sec,
								       sec_edges = sec,
      								 ...) {

  act <- dep <- binding <- binding_input <- binding_output <-
    bindings_input <- bindings_output <- case <-
    color_level <- from_id <- label <-
    ACTIVITY_CLASSIFIER_ <- sec_label <- str_replace <- to_id <- NULL

	extra_data <- list()
	extra_data$n_cases <- length(unique(bindings$case))
	extra_data$n_activity_instances <- length(unique(bindings$aid))

  check_dependencies(dependencies)

  # Build base structure

  is_custom_nodes <- attr(type_nodes, "perspective") == "custom"
  is_custom_edges <- attr(type_edges, "perspective") == "custom"

  if (is_custom_nodes || is_custom_edges) {

    eventlog %>%
      as_tibble() %>%
      group_by(!!sym(case_id(eventlog)),
               !!sym(activity_id(eventlog)),
               !!sym(activity_instance_id(eventlog))) -> log_custom

    if (is_custom_nodes && is_custom_edges) {
      attribute_nodes <- sym(attr(type_nodes, "attribute"))
      attribute_edges <- sym(attr(type_edges, "attribute"))

      log_custom %>%
        summarise(CUSTOM_ATTR_NODES = first(!!attribute_nodes),
                  CUSTOM_ATTR_EDGES = first(!!attribute_edges)) -> log_custom

    } else if (is_custom_nodes) {
      attribute_nodes <- sym(attr(type_nodes, "attribute"))

      log_custom %>%
        summarise(CUSTOM_ATTR_NODES = first(!!attribute_nodes)) -> log_custom
    } else if (is_custom_edges) {
      attribute_edges <- sym(attr(type_edges, "attribute"))

      log_custom %>%
        summarise(CUSTOM_ATTR_EDGES = first(!!attribute_edges)) -> log_custom
    }

    suppressWarnings({
      bindings %>%
        left_join(log_custom, by = c("act" = activity_id(eventlog),
                                     "aid" = activity_instance_id(eventlog),
                                     "case" = case_id(eventlog))) -> bindings
    })
  }

	nodes <- attr(type_nodes, "create_nodes")(bindings, type_nodes, extra_data)
	edges <- attr(type_edges, "create_edges")(dependencies, bindings, type_edges, extra_data)

	# secondary info
	if(!is.null(sec_nodes)) {
		nodes_secondary <- attr(sec_nodes, "create_nodes")(bindings, type_nodes, extra_data) %>%
			select(ACTIVITY_CLASSIFIER_, from_id, label) %>%
			rename(sec_label = label)


		nodes %>%
			full_join(nodes_secondary, by = c("ACTIVITY_CLASSIFIER_", "from_id")) %>%
			mutate(label = if_end(ACTIVITY_CLASSIFIER_,
								  ACTIVITY_CLASSIFIER_,
								  str_replace(paste0(label, "\n","(", map(sec_label, ~str_split(.x, "\n")[[1]][2]), ")"), "\n\\(\\)",""))) -> nodes
	}

	if(!is.null(sec_edges)) {
		edges_secondary <- attr(sec_edges, "create_edges")(dependencies, bindings, type_edges, extra_data) %>%
			select(from_id, to_id, label) %>%
			rename(sec_label = label)

		edges %>%
			full_join(edges_secondary, by = c("from_id","to_id")) %>%
			mutate(label = str_replace(paste0(label, "\n (", sec_label, ')'), "\n \\( \\)","")) -> edges
	}

	cnet <- list(nodes = nodes,
	             edges = edges)

  class(cnet) <- c("causal_net", class(cnet))

	attr(cnet, "dependencies") <- dependencies
	attr(cnet, "bindings") <- bindings
  attr(cnet, "type_nodes") <- type_nodes
  attr(cnet, "type_edges") <- type_edges

	cnet
}

check_dependencies <- function(dependencies) {
	# Check all nodes included in thresholds
	all_acts <- colnames(dependencies)
	connected_acts <- which(dependencies > 0, arr.ind = T)
  acts_antecedent <- rownames(dependencies)[connected_acts[,1]]
  acts_consequent <- colnames(dependencies)[connected_acts[,2]]
  missing_acts <- setdiff(all_acts, union(acts_antecedent, acts_consequent))
  if (length(missing_acts)  > 0) {
    warning(paste0("Activities [",
                   paste(missing_acts, collapse = ","),
                   "] have neither an antecedent or consequent in the supplied dependency matrix.",
                   "Consider using the `all_connected` or `endpoints_connected` parameter when generating the dependency matrix."))
  }

  # Check any node contains a ',' in name
  if (any(grepl(',', all_acts, fixed = T))) {
    warning(paste0("One of the activities contains ',' in its name, which is currently not supported and may lead to problems.."))
  }
}
