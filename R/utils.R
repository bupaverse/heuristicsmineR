#
# Copied from https://github.com/gertjanssenswillen/processmapR (c) Hasselt University under MIT license
# TODO: refactor somehow in a common place?
#

#' @importFrom rlang sym
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
      arrange(!!case_id_(eventlog), !!timestamp_(eventlog), .order) %>%
      # relies on dplyr taking the first distinct value
      distinct(!!case_id_(eventlog), !!activity_id_(eventlog), !!activity_instance_id_(eventlog)) %>%
      rename(case_id = !!case_id_(eventlog),
             activity_id = !!activity_id_(eventlog),
             activity_instance_id = !!activity_instance_id_(eventlog))
}

base_precedence <- function(eventlog) {

  .order <- ACTIVITY_CLASSIFIER_ <- ACTIVITY_INSTANCE_CLASSIFIER_ <-
  CASE_CLASSIFIER_ <- TIMESTAMP_CLASSIFIER_ <- act <- binding <- binding_input <-
  binding_output <- bindings_input <- bindings_output <- case <- color_level <-
  end_time <- from_id <- label <- min_order <- n.x <- n.y <- node_id.x <- node_id.y <-
  start_time <- NULL

	eventlog <- ungroup_eventlog(eventlog)

	eventlog %>%
		as.data.frame() %>%
		droplevels %>%
		select(ACTIVITY_CLASSIFIER_ = !!activity_id_(eventlog),
			   ACTIVITY_INSTANCE_CLASSIFIER_ = !!activity_instance_id_(eventlog),
			   CASE_CLASSIFIER_ = !!case_id_(eventlog),
			   TIMESTAMP_CLASSIFIER_ = !!timestamp_(eventlog),
			   .order) %>%
		group_by(ACTIVITY_CLASSIFIER_, ACTIVITY_INSTANCE_CLASSIFIER_, CASE_CLASSIFIER_) -> grouped_log

	grouped_log %>% summarize(start_time = min(TIMESTAMP_CLASSIFIER_),
							  end_time = max(TIMESTAMP_CLASSIFIER_),
							  min_order = min(.order)) -> base_log

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
		bind_rows(end_points_start, end_points_end, base_log) -> base_log
	)

	#create base nodes list

	base_log %>%
		ungroup() %>%
		distinct(ACTIVITY_CLASSIFIER_) %>%
		mutate(node_id = 1:n()) -> base_nodes

	#create base precedence list

	suppressWarnings(base_log %>%
					 	ungroup() %>%
					 	mutate(ACTIVITY_CLASSIFIER_ = ordered(ACTIVITY_CLASSIFIER_, levels = c("Start", as.character(sort(activity_labels(eventlog))), "End"))) %>%
					 	group_by(CASE_CLASSIFIER_) %>%
					 	arrange(start_time, min_order) %>%
					 	mutate(next_act = lead(ACTIVITY_CLASSIFIER_),
					 		   next_start_time = lead(start_time),
					 		   next_end_time = lead(end_time)) %>%
					 	full_join(base_nodes, by = c("ACTIVITY_CLASSIFIER_" = "ACTIVITY_CLASSIFIER_")) %>%
					 	full_join(base_nodes, by = c("next_act" = "ACTIVITY_CLASSIFIER_")) %>%
					 	ungroup() %>%
					 	select(everything(),
					 		   #-n.x, -n.y,
					 		   from_id = node_id.x,
					 		   to_id = node_id.y) -> base_precedence)

	base_precedence %>%
		rename(case = CASE_CLASSIFIER_,
			   aid = ACTIVITY_INSTANCE_CLASSIFIER_,
			   act = ACTIVITY_CLASSIFIER_) -> base_precedence

	base_precedence
}
