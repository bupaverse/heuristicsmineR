#' Compute input and output bindings
#'
#' Computes the input- and output bindings for use in a causal map. Several heuristics may be used to determine
#' the activities that are activated or consumed by an event. The Flexible Heuristic Miner (FHM) paper describes
#' a heuristic that looks ahead (or looks back) until the end of the trace and determines those activities as activated
#' for which no other cause (activity in a causal dependency) is found. This approach is implemented as type `nearest`.
#'
#' @param eventlog The bupaR event log.
#' @param dependencies A dependency matrix obtained, for example, through \code{\link{dependency_matrix}}.
#' @param type The heuristic used to determine the bindings. Currently only `nearest` is available.
#'
#' @return A data frame
#'
#' @import purrr
#' @import dplyr
#' @export
causal_bindings <- function(eventlog,
                            dependencies,
                            type = c("nearest")) {

  case <- start_time <- min_order <- act <- . <-
    possible_input <- possible_output <- NULL

  stopifnot("eventlog" %in% class(eventlog))
  stopifnot("dependency_matrix" %in% class(dependencies))

  base <- base_precedence_simple(eventlog)

  candidates <- build_candidates(dependencies)
  in_acts <- candidates$in_acts
  out_acts <- candidates$out_acts

  prepared_base <- base %>%
    arrange(case, start_time, min_order) %>%
    filter(!is.na(act)) %>%
    mutate(possible_output = out_acts[act],
           possible_input = in_acts[act]) %>%
    filter_at(vars(possible_input, possible_output),
              all_vars(sapply(., function(x) !is.null(x))))

  act_vec <- prepared_base$act
  rle_ids <- rle(prepared_base$case)
  end = cumsum(rle_ids$lengths)
  start = c(1, lag(end)[-1] + 1)

  out_vec <- prepared_base$possible_output
  binding_output <- pmap(
    .l = list(start, end),
    .f = function(s_idx, e_idx) {
      map(
        .x = (s_idx:e_idx),
        .f = function(i) {
          out_act <- out_vec[[i]] # [[ to simplify to vector
          if (i < e_idx) {
            if (length(out_act) == 1) {
              # only one candidate, assuming that events always have an effect we add it regardless
              out_act
            } else {
              act <- act_vec[i]
              suffix <- act_vec[(i + 1):e_idx]
              res <- get_active(suffix, act, out_act, in_acts[out_act])
              if (length(res) == 0) {
                # No effect may lead to disconnected activitues, so parse again ignoring `in_acts`
                # This is different from what Flexible Heuristics Miner paper does
                get_active(suffix, act, out_act, c(NA))
              } else {
                res
              }
            }
          } else {
            character() # last one is always empty
          }
        }
      )
    }
  )
  binding_output <- unlist(binding_output, recursive = FALSE, use.names = FALSE)

  in_vec <- prepared_base$possible_input
  binding_input <- pmap(
    .l = list(end, start),
    .f = function(s_idx, e_idx) {
      rev(map(
        .x = (s_idx:e_idx),
        .f = function(i) {
          in_act <- in_vec[[i]] # [[ to simplify to vector
          if (i > e_idx) {
            if (length(in_act) == 1) {
              # only one candidate, assuming that events always have a cause we add it regardless
              in_act
            } else {
              act <- act_vec[i]
              prefix <- act_vec[(i - 1):e_idx]
              res <- get_active(prefix, act, in_act, out_acts[in_act])
              if (length(res) == 0) {
                # No cause may lead to disconnected activitues, so parse again ignoring `out_acts`
                # This is different from what Flexible Heuristics Miner paper does
                get_active(prefix, act, in_act, c(NA))
              } else {
                res
              }
            }
          } else {
            character() # first one is always empty
          }
        }
      ))
    }
  )
  binding_input <- unlist(binding_input, recursive = FALSE, use.names = FALSE)

  prepared_base %>%
    mutate(binding_output = binding_output,
           binding_input = binding_input)
}

get_active <- function(suffix, act, candidates, competing) {
  is_nearest <- map2_lgl(candidates, competing, function(cand_act, comp_act) {
    # check whether the binding activity `cand_act` occurs before any possible binding activity `comp_act`
    comp_act <- comp_act[comp_act %in% suffix & comp_act != cand_act]
    cand_idx <- which(suffix == cand_act)
    if (length(cand_idx) > 0) {
      # there are some input bindings
      if (length(comp_act) > 0) {
        all(cand_idx[1] <= which(suffix %in% comp_act))
      } else {
        # no competing activity occuring in suffix
        TRUE
      }
    } else {
      FALSE
    }
  })
  candidates[is_nearest]
}

build_candidates <- function(d) {

  succ <- function(act) {
    names(which(d[act,] > 0))
  }
  pred <- function(act) {
    names(which(d[,act] > 0))
  }

  # build lookup lists
  in_acts <- map(colnames(d), pred)
  names(in_acts) <- colnames(d)
  out_acts <- map(colnames(d), succ)
  names(out_acts) <- colnames(d)

  list(in_acts = in_acts,
       out_acts = out_acts)
}
