#' Converts the object to a Petrinet
#'
#' @param obj The event log to be used. An object of class
#'
#' @examples
#' data(L_heur_1)
#' cn <- causal_net(L_heur_1, threshold = .8)
#' pn <- as.petrinet(cn)
#' petrinetR::render_PN(pn)
#'
#' @import dplyr
#' @export
as.petrinet <- function(obj) {
	UseMethod("as.petrinet")
}

#' @export
as.petrinet.causal_net <- function(obj) {
  to <- from  <- NULL

  edges <- obj$edges
  nodes <- obj$nodes

  act_trans <- nodes$act
  inv_trans <- c()

  in_places <- paste0("p_in_", seq_along(act_trans))
  out_places <- paste0("p_out_", seq_along(act_trans))
  gate_places <- c()

  act_places <- c(in_places, out_places)
  act_flow <- data.frame(from = c(in_places, act_trans),
                         to = c(act_trans, out_places), stringsAsFactors = F)

  marking = act_flow[act_flow$from == "Start",]$to

  inv_count <- 0
  binding_flow <- imap_dfr(nodes$act, function(x,i) {

    input <- unlist(nodes[i,]$bindings_input)
    in_flows <- map_dfr(names(input), function(x) { # with side effects !!

      inv_count <<- inv_count + 1
      inv_name <- paste0("inv_",inv_count)
      inv_trans <<- c(inv_trans, inv_name)

      in_place <- paste0("p_in_", i)

      # Join
      in_acts <- unlist(strsplit(x, ","))
      map_dfr(in_acts, function(y) {
        in_idx <- which(nodes$act == y)
        gateway_name <- paste0("p_gw_",in_idx,"_",i)
        gate_places <<- c(gate_places, gateway_name)
        data.frame(from = c(gateway_name, inv_name),
                   to = c(inv_name, in_place),
                   stringsAsFactors = F)
      })
    })

    output <- unlist(nodes[i,]$bindings_output)
    out_flows <- map_dfr(names(output), function(x) { # with side effects !!

      inv_count <<- inv_count + 1
      inv_name <- paste0("inv_",inv_count)
      inv_trans <<- c(inv_trans, inv_name)

      out_place <- paste0("p_out_", i)

      # Split
      in_acts <- unlist(strsplit(x, ","))
      map_dfr(in_acts, function(y) {
        out_idx <- which(nodes$act == y)
        gateway_name <- paste0("p_gw_",i,"_",out_idx)
        gate_places <<- c(gate_places, gateway_name)
        data.frame(from = c(inv_name, out_place),
                   to = c(gateway_name, inv_name),
                   stringsAsFactors = F)
      })
    })

    bind_rows(in_flows,out_flows)

  })


  trans_final <- tibble(id = setdiff(union(act_trans, inv_trans), c("Start", "End"))) %>%
    mutate(label = id)
  places_final <- tibble(id = union(gate_places,
                        setdiff(act_places,
                                c(act_flow[act_flow$to == "Start",]$from,
                                  act_flow[act_flow$from == "End",]$to))))  %>%
    mutate(label = id)

  flows_final <- bind_rows(act_flow, binding_flow) %>%
    distinct() %>%
    filter(!(to %in% c("Start", "End")),
           !(from %in% c("Start", "End")))

  # TODO simplify Petri net by removing irrelevant invisible transitions

  pn <- petrinetR::create_PN(places_final,
                       trans_final,
                       flows_final)

  pn$transitions <- pn$transitions %>%
    mutate(label = if_else(startsWith(id,"inv_"), NA_character_, id))

  pn
}
