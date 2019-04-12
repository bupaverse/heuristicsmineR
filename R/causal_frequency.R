#
# Based on code from https://github.com/gertjanssenswillen/processmapR
# (c) Hasselt University under MIT license
#

#' @title Frequency map profile
#' @description Function to create a frequency profile for a process map.
#'
#' @param value The type of frequency value to be used:
#' absolute, relative (percentage of activity instances).
#' @param color_scale Name of color scale to be used for nodes. Defaults to PuBu. See `Rcolorbrewer::brewer.pal.info()` for all options.
#' @param color_edges The color used for edges. Defaults to dodgerblue4.
#'
#' @examples
#' causal_net(L_heur_1,
#'            type = causal_frequency("relative"))
#'
#' @export
causal_frequency <- function(value = c("absolute", "relative"),
                      color_scale = "PuBu",
                      color_edges = "dodgerblue4") {

  act <- binding <- binding_input <- binding_output <- bindings_input <-
    bindings_output <- case <- from_id <- label <- NULL

  value <- match.arg(value)
	attr(value, "perspective") <- "frequency"
	attr(value, "color") <- color_scale
	attr(value, "color_edges") <- color_edges

	attr(value, "create_nodes") <- function(bindings, type, extra_data) {

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
  		group_by(act, from_id) %>%
  		summarize(n = as.double(n()),
  				  n_distinct_cases = as.double(n_distinct(case))) %>%
  		ungroup() %>%
      left_join(nested_input,  by = c("act" = "act")) %>%
      rename(bindings_input = bindings) %>%
      left_join(nested_output,  by = c("act" = "act")) %>%
      rename(bindings_output = bindings) %>%
      mutate(bindings_input = map(bindings_input, replace_null),
             bindings_output = map(bindings_output, replace_null)) %>%
      mutate(label = case_when(type == "relative" ~ 100*n/n_activity_instances,
              								 type == "absolute" ~ n,
              								 type == "absolute_case" ~ n_distinct_cases,
              								 type == "relative_case" ~ 100*n_distinct_cases/n_cases)) %>%
  		mutate(color_level = label,
    			   shape = if_end(act,"circle","rectangle"),
    			   fontcolor = if_end(act, if_start(act, "chartreuse4","brown4"),  ifelse(label <= (min(label) + (5/8)*diff(range(label))), "black","white")),
    			   color = if_end(act, if_start(act, "chartreuse4","brown4"),"black"),
    			   label = if_end(act, act, paste0(act, "\n",
              			                    round(label, 2),
              			                    ifelse(type %in% c("absolute", "absolute_case"),"", "%"))),
    			   tooltip = paste0("Input: ", format_bindings(bindings_input), "\n",
    			                    "Output: ", format_bindings(bindings_output))) %>%
  		na.omit()

    attr(nodes, "bindings_input") <- nested_input
    attr(nodes, "bindings_output") <- nested_output

    nodes
	}

	attr(value, "create_edges") <- function(dependencies, bindings, type, extra_data) {
		n_cases <- extra_data$n_cases
		n_activity_instances <- extra_data$n_activity_instances

    summary_freq <- bindings %>%
      select(act, binding_output) %>%
      filter(lengths(binding_output) > 0) %>%
      tidyr::unnest(binding_output) %>%
      count(act, binding_output) %>%
      rename(binding = binding_output)

    base_nodes <- bindings %>%
      group_by(act) %>%
      summarise(id = first(from_id))

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
        mutate(n = as.double(if_else(is.na(n), 0L, n))) %>%
  			mutate(label = case_when(type == "relative" ~ round(100*n/sum(n),2),
  									             type == "absolute" ~ n)) %>%
  			ungroup() %>%
  			mutate(penwidth = scales::rescale(label, to = c(1,5), from = c(0, max(label)))) %>%
  			mutate(label = case_when(type == "absolute" ~ paste0(label, ""),
  									             type == "relative" ~ paste0(label, "%")))
    )

    edges
	}
	return(value)
}

