
#' Table of cost-effectiveness statistics
#'
#' Take \code{BCEA} package object as input and
#' creates a policy summary table over scenarios.
#'
#' @param bcea_out Pre-calculated output from BCEA package
#' @param wtp_threshold Willingness to pay; Default: £20,000
#' @param ncohort hypothetical cohort size. This scales up small values to something more meaningful.
#' @param folder text string save location
#'
#' @return data.frame with columns:
#' \itemize{
#'   \item Cost
#'   \item QALY
#'   \item Incremental_cost
#'   \item Incremental_QALY
#'   \item ICER
#'   \item INB
#'   \item ceac_WTP20k
#'   \item ceac_WTP25k
#'   \item ceac_WTP30k
#' }
#'
#' @export
#' @examples
#'
#' ce_res <- combine_popmod_dectree_res(cohort, interv,
#'                                      popmod_res, dectree_res)
#' s_bcea <- screen_bcea(ce_res$ce_incr)
#' table_costeffectiveness(s_bcea)
#'
table_costeffectiveness <- function(bcea, ...) {

  UseMethod("table_costeffectiveness", bcea)
}


#' @rdname table_costeffectiveness
#'
table_costeffectiveness.bcea <- function(bcea_out,
                                         wtp_threshold = 20000,
                                         ncohort = 1000,
                                         folder = NA) {

  mean_vals <-
    with(bcea_out,
         do.call(data.frame,
                 list(
                   "Scenario" = seq_len(n.comparators) - 1,
                   "Cost" = round(colMeans(c), 2)*ncohort,
                   "QALY" = round(colMeans(e), 5)*ncohort,
                   "Incremental_cost" = c("", round(colMeans(delta.c), 2)*ncohort),
                   "Incremental_QALY" = c("", round(colMeans(delta.e), 5)*ncohort),
                   "ICER" = c("", round(ICER, 2)),
                   "INB" = c("", round(eib[k == wtp_threshold], 2)),
                   "ceac_WTP15k" = c("", round(ceac[k == 15000, ], 2)),
                   "ceac_WTP20k" = c("", round(ceac[k == 20000, ], 2)),
                   "ceac_WTP25k" = c("", round(ceac[k == 25000, ], 2)),
                   "ceac_WTP30k" = c("", round(ceac[k == 30000, ], 2)))))

  L95 <-
    with(bcea_out,
         do.call(data.frame,
                 list(
                   "Scenario" = seq_len(n.comparators) - 1,
                   "Cost" = round(colQuantiles(c, probs = 0.025), 2)*ncohort,
                   "QALY" = round(colQuantiles(e, probs = 0.025), 5)*ncohort,
                   "Incremental_cost" = c("", round(colQuantiles(delta.c, probs = 0.025), 2)*ncohort),
                   "Incremental_QALY" = c("", round(colQuantiles(delta.e, probs = 0.025), 5)*ncohort),
                   "ICER" = rep("", length(n.comparators)), # how to get this??
                   "INB" = c("", round(colQuantiles(ib[k == wtp_threshold, , ], probs = 0.025), 2)),
                   "ceac_WTP15k" = rep("", length(n.comparators)),
                   "ceac_WTP20k" = rep("", length(n.comparators)),
                   "ceac_WTP25k" = rep("", length(n.comparators)),
                   "ceac_WTP30k" = rep("", length(n.comparators))
                 )))

  U95 <-
    with(bcea_out,
         do.call(data.frame,
                 list(
                   "Scenario" = seq_len(n.comparators) - 1,
                   "Cost" = round(colQuantiles(c,probs = 0.975), 2)*ncohort,
                   "QALY" = round(colQuantiles(e, probs = 0.975), 5)*ncohort,
                   "Incremental_cost" = c("", round(colQuantiles(delta.c, probs = 0.975), 2)*ncohort),
                   "Incremental_QALY" = c("", round(colQuantiles(delta.e, probs = 0.975), 5)*ncohort),
                   "ICER" = rep("", length(n.comparisons)), # how to get this??
                   "INB" = c("", round(colQuantiles(ib[k == wtp_threshold, , ], probs = 0.975), 2)),
                   "ceac_WTP15k" = rep("", length(n.comparators)),
                   "ceac_WTP20k" = rep("", length(n.comparators)),
                   "ceac_WTP25k" = rep("", length(n.comparators)),
                   "ceac_WTP30k" = rep("", length(n.comparators))
                 )))

  # make 95% CI "(., .)"
  LU95 <-
    rbind(L95,
          U95) %>%
    group_by(Scenario) %>%
    summarise(Cost = paste("(", paste(Cost, collapse = ", "), ")", sep = ""),
              QALY = paste("(", paste(QALY, collapse = ", "), ")", sep = ""),
              Incremental_cost = paste("(", paste(Incremental_cost, collapse = ", "), ")", sep = ""),
              Incremental_QALY = paste("(", paste(Incremental_QALY, collapse = ", "), ")", sep = ""),
              ICER = paste(ICER, collapse = ""),
              INB = paste("(", paste(INB, collapse = ", "), ")", sep = ""),
              ceac_WTP15k = paste(ceac_WTP15k, collapse = ""),
              ceac_WTP20k = paste(ceac_WTP20k, collapse = ""),
              ceac_WTP25k = paste(ceac_WTP25k, collapse = ""),
              ceac_WTP30k = paste(ceac_WTP30k, collapse = "")) %>%
    mutate(Scenario)

  # combine all ". (., .)"
  res <-
    rbind(mean_vals,
          LU95) %>%
    group_by(Scenario) %>%
    summarise(Cost = paste(Cost, collapse = " "),
              QALY = paste(QALY, collapse = " "),
              Incremental_cost = paste(Incremental_cost, collapse = " "),
              Incremental_QALY = paste(Incremental_QALY, collapse = " "),
              ICER = paste(ICER, collapse = ""),
              INB =  paste(INB, collapse = " "),
              ceac_WTP15k = paste(ceac_WTP15k, collapse = ""),
              ceac_WTP20k = paste(ceac_WTP20k, collapse = ""),
              ceac_WTP25k = paste(ceac_WTP25k, collapse = ""),
              ceac_WTP30k = paste(ceac_WTP30k, collapse = ""))

  if (!is.na(folder)) {
    write.csv(x = res,
              file = paste(folder, "ce_table.csv", sep = "/"))
  }

  invisible(res)
}
