
#' Calculate Numbers of Active TB Cases using Hazards
#'
#' Calculate the number tb_uk (extrapolated) using yearly hazards.
#  Uses compartmental type aggregation.
#'
#' ##TODO: make dependent on different LTBI probs
#'
#' @param strat_pop
#' @param hazard
#'
#' @return
#' @export
#'
#' @examples
#'
calc_num_active_tb <- function(strat_pop,
                               hazard) {

  prob_LTBI <- 0.3
  num_uk_tb <- NULL

  # yearly in uk LTBI population
  # depend on all other competing risks

  LTBI_pop <- strat_pop["at-risk", ] * prob_LTBI

  for (i in seq_along(LTBI_pop[-1])) {

    num_uk_tb[i + 1] <- hazard[i + 1] * LTBI_pop[i]

    # removing estimated active tb from risk set
    # replace observed active tb

    LTBI_pop[i + 1] <- LTBI_pop[i + 1] - num_uk_tb[i + 1] + strat_pop["tb", i + 1]
  }

  return(num_uk_tb)
}
