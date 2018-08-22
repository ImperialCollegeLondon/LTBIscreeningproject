
#' Net monetary benefit matrix
#'
#' Create input data for regressions.
#'
#' This is _not_ incremental benefit.
#'
#' @param ce1 interventions
#' @param ce0 status-quo
#' @param folders list of strings
#' @param design_mat
#' @param wtp_min numeric
#' @param wtp_max numeric
#'
#' @return list by wtp
#' @export
#'
#' @examples
#'
nmb_matrix <- function(ce1,
                       ce0,
                       folders = NA,
                       design_mat = NA,
                       wtp_min = 10000,
                       wtp_max = 30000) {

  if (!all(is.na(folders))) {
    design_mat <-
      pastef(folders$output$parent,
             "scenario_params_df.csv") %>%
      read.csv() %>%
      design_matrix()
  }

  wtp_seq <- seq(wtp_min, wtp_max, by = 10000)

  nmb_wtp <-
    lapply(wtp_seq,
           FUN = function(wtp) nmb_scenarios(ce0$e, ce0$c,
                                             ce1$e, ce1$c,
                                             wtp)) %>%
    do.call(what = rbind, args = .)

  # join inputs and outputs
  nmb_mat <-
    merge(x = design_mat,
          y = nmb_wtp,
          by = "scenario") %>%
    mutate(type = factor(type,
                         levels = c("statusquo", "screened"))) %>%
    arrange(scenario, wtp, type)

  if (!all(is.na(folders))) {
    save(nmb_mat,
         file = pastef(folders$output$scenario, "sim_matrix.RData"))
  }

  # return as list
  nmb_mat <- split(nmb_mat, nmb_mat$wtp)

  return(nmb_mat)
}


#' nmb_matrix_tb
#'
#' @param aTB_CE_stats
#' @param ...
#'
#' @return
#' @export
#'
nmb_matrix_tb <- function(aTB_CE_stats,
                          ...) {

  ce0 <- make_ce0(aTB_CE_stats)
  ce1 <- make_ce1(aTB_CE_stats)

  nmb_matrix(ce1, ce0, ...)
}
