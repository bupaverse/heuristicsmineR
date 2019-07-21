#
# Based on code from https://github.com/gertjanssenswillen/processmapR
# (c) Hasselt University under MIT license
#

#' @title Custom map profile
#' @description Function to create a custom map profile based on some event log attribute.
#' @details If used for edges, it will show the attribute values which related to the out-going node of the edge.#'
#' @param FUN A summary function to be called on the process time of a specific activity, e.g. mean, median, min, max
#' @param attribute The name of the case attribute to visualize (should be numeric)
#' @param units Character to be placed after values (e.g. EUR for monitary euro values)
#' @param color_scale Name of color scale to be used for nodes. Defaults to RdPu See `Rcolorbrewer::brewer.pal.info()` for all options.
#' @param color_edges The color used for edges. Defaults to red4.
#' @param ... Additional arguments forwarded to FUN
#'
#' @examples
#' causal_net(L_heur_1,
#'            type_nodes = causal_custom(attribute = "timestamp"),
#'            type_edges = causal_custom(attribute = "timestamp"))
#'
#' @export
causal_custom <- function(FUN = mean,
                          attribute,
                          units = "",
                          color_scale = "RdPu",
                          color_edges = "red4",
                          ...) {

  act <- binding <- binding_input <- binding_output <- bindings_input <-
    bindings_output <- case <- from_id <- label <- NULL

  attr(FUN, "attribute") <- attribute
  units <- match.arg(units)
	attr(FUN, "perspective") <- "custom"
	attr(FUN, "units_label") <- units
	attr(FUN, "arguments") <- list(...)
	attr(FUN, "color") <- color_scale
	attr(FUN, "color_edges") <- color_edges

	attr(FUN, "create_nodes") <- function(bindings, type, extra_data) {

	  . <- duration <-  end_time <- start_time <- tooltip <-
	    CUSTOM_ATTR_NODES <- NULL

	  n_cases <- extra_data$n_cases
		n_activity_instances <- extra_data$n_activity_instances

    nested_output <- bindings %>%
      mutate(binding = as.character(map(binding_output, paste, collapse=","))) %>%
      count(act, binding) %>%
      filter(binding != "") %>% # remove bindings without any activity activated
      group_by(act) %>%
      summarise(bindings = list(map2(binding, n, function(x,y) {
        names(y) <- x
        y
      })))

    nested_input <- bindings %>%
      mutate(binding = as.character(map(binding_input, paste, collapse=","))) %>%
      count(act, binding) %>%
      filter(binding != "") %>% # remove bindings without any activity activated
      group_by(act) %>%
      summarise(bindings = list(map2(binding, n, function(x,y) {
        names(y) <- x
        y
      })))

    nodes <- bindings %>%
      left_join(nested_input,  by = c("act" = "act")) %>%
      rename(bindings_input = bindings) %>%
      left_join(nested_output,  by = c("act" = "act")) %>%
      rename(bindings_output = bindings) %>%
      mutate(bindings_input = map(bindings_input, replace_null),
             bindings_output = map(bindings_output, replace_null)) %>%
      group_by(act, from_id) %>%
			summarize(bindings_input = first(bindings_input),
			          bindings_output = first(bindings_output),
			          label = do.call(function(...) type(CUSTOM_ATTR_NODES, na.rm = T,...),  attr(type, "arguments"))) %>%
      mutate(label = if_end(act, 0, if_start(act, 0, label))) %>%
			na.omit() %>%
			ungroup() %>%
			mutate(color_level = label,
				   shape = if_end(act,"circle","rectangle"),
				   fontcolor = if_end(act, if_start(act, "chartreuse4","brown4"),  ifelse(label <= (min(label) + (4/8)*diff(range(label))), "black","white")),
				   color = if_end(act, if_start(act, "chartreuse4","brown4"),"grey"),
				   tooltip = paste0(act, "\n", round(label, 2), " ",attr(type, "units_label")),
				   label = if_end(act, act, tooltip),
				   tooltip = paste0("Input: ", format_bindings(bindings_input), "\n",
				                    "Output: ", format_bindings(bindings_output))) %>%
  		na.omit()

    attr(nodes, "bindings_input") <- nested_input
    attr(nodes, "bindings_output") <- nested_output

    nodes
	}

	attr(FUN, "create_edges") <- function(dependencies, bindings, type, extra_data) {

	  . <- end_time <- min_order <- start_time <- value <- waiting_time <-
	    CUSTOM_ATTR_EDGES <- NULL

	  n_cases <- extra_data$n_cases
		n_activity_instances <- extra_data$n_activity_instances

    unnested_bindings <- bindings %>%
      select(case, act, min_order, start_time, end_time, binding_output, CUSTOM_ATTR_EDGES) %>%
      filter(lengths(binding_output) > 0) %>%
      tidyr::unnest(binding_output) %>%
      arrange(case, start_time, min_order)

    summary_freq <- unnested_bindings %>%
      group_by(act, binding_output) %>%
      summarise(n = n(),
                value = do.call(function(...) type(CUSTOM_ATTR_EDGES, na.rm = T,...),  attr(type, "arguments"))) %>%
      mutate(value = if_end(act, 0, if_start(act, 0, label))) %>%
      rename(binding = binding_output)

    base_nodes <- bindings %>%
      distinct(act, from_id) %>%
      rename(id = from_id)

    suppressWarnings( # factor / char coercion
      edges <- dependencies %>%
        as.data.frame() %>%
        left_join(base_nodes, by = c("antecedent" = "act")) %>%
        rename(from_id = id) %>%
        left_join(base_nodes, by = c("consequent" = "act")) %>%
        rename(to_id = id) %>%
        left_join(summary_freq,
                  by = c("antecedent" = "act", "consequent" = "binding")) %>%
        na.omit() %>%
  			group_by(from_id) %>%
        mutate(label_numeric = value) %>%
        mutate(n = as.double(if_else(is.na(n), 0L, n))) %>%
        mutate(label = if_else(value == 0,
                               " ",
                               paste0(round(value,2), " ",
                                      attr(type, "units_label")))) %>%
  			ungroup() %>%
        mutate(penwidth = scales::rescale(value, to = c(1,5)))
    )

    edges
	}
	return(FUN)
}

