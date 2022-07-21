#
# Copied from https://github.com/gertjanssenswillen/processmapR (c) Hasselt University under MIT license
# TODO: refactor somehow in a common place?
#

#' @importFrom rlang sym
#' @importFrom data.table data.table
#' @importFrom data.table :=
#' @import dplyr
NULL

case_id_ <- function(eventlog) sym(case_id(eventlog))
activity_id_ <- function(eventlog) sym(activity_id(eventlog))
activity_instance_id_ <- function(eventlog) sym(activity_instance_id(eventlog))
resource_id_ <- function(eventlog) sym(resource_id(eventlog))
timestamp_ <- function(eventlog) sym(timestamp(eventlog))
lifecycle_id_ <- function(eventlog) sym(lifecycle_id(eventlog))


if_end <- function(node, true, false) {
	ifelse(node %in% c("Start","End"), true, false)
}
if_start <- function(node, true, false) {
	ifelse(node %in% c("Start"), true, false)
}

reduce_simple_eventlog <- function(eventlog) {
  .order <- NULL

  eventlog %>%
      as.data.frame() %>%
      droplevels() %>%
      arrange(!!case_id_(eventlog), !!timestamp_(eventlog), .order) %>%
      # relies on dplyr taking the first distinct value
      distinct(!!case_id_(eventlog), !!activity_id_(eventlog), !!activity_instance_id_(eventlog)) %>%
      rename(case_id = !!case_id_(eventlog),
             activity_id = !!activity_id_(eventlog),
             activity_instance_id = !!activity_instance_id_(eventlog)) %>%
      mutate(activity_id = as.factor(activity_id)) # fix for bupaR 0.5 since activity_id is not necessarily a factor anymore
}

reduce_activitylog <- function(eventlog) {
  .order <- lifecycle <- min_order <- max_order <- NULL

	eventlog %>%
		as.data.frame() %>%
		droplevels %>%
		select(activity_id = !!activity_id_(eventlog),
			   activity_instance_id = !!activity_instance_id_(eventlog),
			   case_id = !!case_id_(eventlog),
			   timestamp = !!timestamp_(eventlog),
			   .order) %>%
	  mutate(activity_id = as.factor(activity_id)) -> prepared_log # fix for bupaR 0.5 since activity_id is not necessarily a factor anymore


	data.table::setDT(prepared_log)
	prepared_log[, list(start = min(timestamp),
						complete = max(timestamp),
						min_order = min(.order),
						max_order = max(.order)),
				 by = c("activity_id", "activity_instance_id", "case_id")] %>%
	  data.table::melt(measure.vars = c("start", "complete"), variable.name = "lifecycle", value.name="timestamp") -> reduced_log

	reduced_log[, .order := if_else(lifecycle == "start", min_order, max_order)][
    , c("min_order","max_order") := NULL]
	data.table::setorderv(reduced_log, c("case_id", "timestamp", ".order"))

	return(reduced_log)
}

base_precedence_simple <- function(eventlog) {

  .order <- ACTIVITY_CLASSIFIER_ <- ACTIVITY_INSTANCE_CLASSIFIER_ <-
  CASE_CLASSIFIER_ <- TIMESTAMP_CLASSIFIER_ <- act <- next_act <- case <-
  from_id <- label <- node_id.x <- node_id.y <- start_time <- end_time <-
    n.x <- n.y <- min_order <- NULL

	eventlog <- ungroup_eventlog(eventlog)

	eventlog %>%
		as.data.frame() %>%
		droplevels() %>%
		select(ACTIVITY_CLASSIFIER_ = !!activity_id_(eventlog),
			   ACTIVITY_INSTANCE_CLASSIFIER_ = !!activity_instance_id_(eventlog),
			   CASE_CLASSIFIER_ = !!case_id_(eventlog),
			   TIMESTAMP_CLASSIFIER_ = !!timestamp_(eventlog),
			   everything(),
			   .order) -> prepared_log

	data.table::setDT(prepared_log)
	prepared_log[, list(start_time = min(TIMESTAMP_CLASSIFIER_),
	                    end_time = max(TIMESTAMP_CLASSIFIER_),
	                    min_order = min(.order)),
	             by = c("ACTIVITY_CLASSIFIER_", "ACTIVITY_INSTANCE_CLASSIFIER_", "CASE_CLASSIFIER_")] %>%
	  as.data.frame() -> base_log

	#create end points for graph

	base_log %>%
		group_by(CASE_CLASSIFIER_) %>%
		arrange(start_time, min_order) -> points_temp

	points_temp %>%
		slice(1) %>%
		mutate(ACTIVITY_CLASSIFIER_ = "Start",
			   end_time = start_time,
			   min_order = -Inf) -> end_points_start
	points_temp %>%
		slice(n()) %>%
		mutate(ACTIVITY_CLASSIFIER_ = "End",
			   start_time = end_time,
			   min_order = Inf) -> end_points_end

	#add endpoints to base log

	suppressWarnings(
		bind_rows(end_points_start, end_points_end, base_log) %>%
			ungroup() -> base_log
	)

	base_log %>%
		count(ACTIVITY_CLASSIFIER_) %>%
		mutate(node_id = 1:n()) -> base_nodes
	data.table::setDT(base_nodes, key = c("ACTIVITY_CLASSIFIER_"))

	#create base precedence list

	data.table::setDT(base_log, key = c("start_time", "min_order"))
	base_log[, ACTIVITY_CLASSIFIER_ := ordered(ACTIVITY_CLASSIFIER_,
											   levels = c("Start", as.character(sort(activity_labels(eventlog))), "End"))
	      	][, `:=`(next_act = data.table::shift(ACTIVITY_CLASSIFIER_, 1, type = "lead"),
	      			 next_start_time = data.table::shift(start_time, 1, type = "lead"),
	      			 next_end_time = data.table::shift(end_time, 1, type = "lead")),
	      	  by = CASE_CLASSIFIER_] %>%
	 	merge(base_nodes, by.x = c("ACTIVITY_CLASSIFIER_"), by.y = c("ACTIVITY_CLASSIFIER_"), all = TRUE) %>%
	 	merge(base_nodes, by.x = c("next_act"), by.y = c("ACTIVITY_CLASSIFIER_"), all = TRUE) %>%
		as.data.frame() %>%
	 	select(everything(),
	 		   -n.x, -n.y,
	 		   from_id = node_id.x,
	 		   to_id = node_id.y) %>%
    rename(case = CASE_CLASSIFIER_,
           aid = ACTIVITY_INSTANCE_CLASSIFIER_,
           act = ACTIVITY_CLASSIFIER_) -> base_precedence

	base_precedence
}

format_bindings <- function(bindings) {
  lapply(bindings, function(x) {
    x <- unlist(x)
    if (length(x) > 0) {
      paste0(unname(x), "x [", names(x), "]", collapse = ", ")
    } else {
      "[]"
    }
  })
}

replace_null <- function(x, replacement = list()) {
  if (is.null(x)) {
    replacement
  } else {
    x
  }
}
