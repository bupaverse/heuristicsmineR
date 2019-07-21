#
# Based on code from https://github.com/gertjanssenswillen/processmapR
# (c) Hasselt University under MIT license
#

#' @title Performance map profile
#' @description Function to create a performance profile for a causal map.
#'
#' @param FUN A summary function to be called on the process time of a specific activity, e.g. mean, median, min, max
#' @param units The time unit in which processing time should be presented (mins, hours, days, weeks, months, quarters, semesters, years. A month is defined as 30 days. A quarter is 13 weeks. A semester is 26 weeks and a year is 365 days
#' @param color_scale Name of color scale to be used for nodes. Defaults to Reds. See `Rcolorbrewer::brewer.pal.info()` for all options.
#' @param color_edges The color used for edges. Defaults to red4.
#' @param ... Additional arguments forwarded to FUN
#'
#' @examples
#' causal_net(L_heur_1,
#'            type = causal_performance())
#'
#' @export
causal_performance <- function(FUN = mean,
                               units = c("mins","secs", "hours","days","weeks", "months", "quarters", "semesters","years"),
                               color_scale = "Reds",
                               color_edges = "red4",
                               ...) {

  act <- binding <- binding_input <- binding_output <- bindings_input <-
    bindings_output <- case <- from_id <- label <- NULL

  units <- match.arg(units)
	attr(FUN, "perspective") <- "performance"
	attr(FUN, "units_label") <- units
	attr(FUN, "arguments") <- list(...)

	if(units %in% c("mins","hours","days","weeks", "secs")) {
		attr(FUN, "units") <- units
		attr(FUN, "scale_time") <- 1
	} else if (units == "months") {
		attr(FUN, "units") <- "days"
		attr(FUN, "scale_time") <- 1/30
	} else if (units == "semesters") {
		attr(FUN, "units") <- "days"
		attr(FUN, "scale_time") <- 1/(26*7)
	}
	else if (units == "years") {
		attr(FUN, "units") <- "days"
		attr(FUN, "scale_time") <- 1/(365)
	} else if(units == "quarters") {
		attr(FUN, "units") <- "days"
		attr(FUN, "scale_time") <- 1/(13*7)
	}

	attr(FUN, "color") <- color_scale
	attr(FUN, "color_edges") <- color_edges

	attr(FUN, "create_nodes") <- function(bindings, type, extra_data) {

	  . <- duration <-  end_time <- start_time <- tooltip <- NULL

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
			mutate(duration = as.double(end_time-start_time, units = attr(type, "units"))*attr(type, "scale_time")) %>%
			na.omit() %>%
      group_by(act, from_id) %>%
			summarize(bindings_input = first(bindings_input),
			          bindings_output = first(bindings_output),
			          label = do.call(function(...) type(duration, na.rm = T,...),  attr(type, "arguments"))) %>%
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

	  . <- end_time <- min_order <- start_time <- value <- waiting_time <- NULL

	  n_cases <- extra_data$n_cases
		n_activity_instances <- extra_data$n_activity_instances

    unnested_bindings <- bindings %>%
      select(case, act, min_order, start_time, end_time, binding_output) %>%
      filter(lengths(binding_output) > 0) %>%
      tidyr::unnest(binding_output) %>%
      arrange(case, start_time, min_order)

    rle_ids <- rle(unnested_bindings$case)
    end = cumsum(rle_ids$lengths)
    start = c(1, lag(end)[-1] + 1)

    # TODO improve the performance of this part
    summary_freq <- unnested_bindings %>%
      mutate(waiting_time = unlist(pmap(
        .l = list(start, end),
        .f = function(s_idx, e_idx) {
          map_dbl(
            .x = (s_idx:e_idx),
            .f = function(i) {
              if (i %in% c(s_idx, e_idx)) {
                # no time from/until artifical start/end
                return(0.0)
              }
              current_binding <- .$binding_output[i]
              if (current_binding == "End") {
                return(0.0) # no time until artifical end
              }
              current_end <- .$end_time[i]
              suffix_act <- .$act[(i + 1):e_idx]
              suffix_start <- .$start_time[(i + 1):e_idx]
              next_start <-
                suffix_start[match(current_binding, suffix_act)]
              if (is.na(next_start)) {
                warning(
                  paste0(
                    "Dependant activity ",
                    current_binding,
                    " not found in \n",
                    paste(suffix_act, suffix_start, collapse = "\n"),
                    "\n",
                    "Returning NA!"
                  )
                )
                return(NA)
              } else {
                wait <- as.double(next_start - current_end,
                                  units = attr(type, "units")) * attr(type, "scale_time")
                if (wait >= 0) {
                  return(wait)
                } else {
                  warning(
                    paste0(
                      "Dependant activity ",
                      current_binding,
                      " has negative waiting time \n",
                      "End of activity: ",
                      current_end,
                      "\n",
                      paste(suffix_act, suffix_start, collapse = "\n"),
                      "\n",
                      "Returning 0!"
                    )
                  )
                  return(0)
                }
              }
            }
          )
        }
      ))) %>%
      group_by(act, binding_output) %>%
      summarise(n = n(),
                value = do.call(function(...) type(waiting_time, na.rm = T,...),  attr(type, "arguments"))) %>%
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

