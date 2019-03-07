#
# Partly based on code from https://github.com/gertjanssenswillen/processmapR
# (c) Hasselt University released under MIT license
#

#' Create a Causal net (or Heuristics net)
#'
#' Creates a Causal net, also known as Heuristics net. This is similar to a processmapR process map.
#' However, the causal map deals with parallelism by trying to identifying causal dependencies
#' between activities using several heuristics.
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
#' \dontrun{
#' # The returned DiagrammeR object can be further augmented with
#' # panning and zooming before rendering:
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
                                                        ...),
                       bindings = causal_bindings(eventlog,
                                                  dependencies),
                       threshold = 0.9,
                       type = causal_frequency("absolute"),
      								 type_nodes = type,
      								 type_edges = type,
      								 ...) {

  act <- dep <- binding <- binding_input <- binding_output <-
    bindings_input <- bindings_output <- case <-
    color_level <- from_id <- label <- NULL

	extra_data <- list()
	extra_data$n_cases <- length(unique(bindings$case))
	extra_data$n_activity_instances <- length(unique(bindings$aid))

  check_dependencies(dependencies)

  # Build base structure
	nodes <- attr(type_nodes, "create_nodes")(bindings, type_nodes, extra_data)
	edges <- attr(type_edges, "create_edges")(dependencies, bindings, type_edges, extra_data)

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
                   "Consider using the `all_connected` parameter when generating the dependency matrix."))
  }

  # Check any node contains a ',' in name
  if (any(grepl(',', all_acts, fixed = T))) {
    warning(paste0("One of the activities contains ',' in its name, which is currently not supported and may lead to problems.."))
  }
}
