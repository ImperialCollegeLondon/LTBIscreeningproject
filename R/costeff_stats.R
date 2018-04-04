
#' Cost-effectiveness Statistics
#'
#' @param scenario_dat list
#' @param interv_QALY
#' @param interv_cost
#' @param pop_year
#'
#' @return
#' @export
#'
#' @examples

costeff_stats <- function(scenario_dat,
                          interv_QALY,
                          interv_cost,
                          pop_year) {

  interv_cost_vs <-
    interv_cost %>%
    purrr::transpose() %>%
    simplify_all()

  interv_QALY_vs <-
    interv_QALY %>%
    purrr::transpose() %>%
    simplify_all()

  res <-
    list(

      #########
      # costs #
      #########

      QALY.statusquo = map(interv_QALY_vs, "statusquo"),
      QALY.screened = map(interv_QALY_vs, "screened"),

      E_cost_screened = mean(interv_cost_vs$screened),
      cost.screened_person = interv_cost_vs$screened/pop_year,
      cost.statusquo_person = interv_cost_vs$statusquo/pop_year,
      cost_incur = interv_cost_vs$screened - interv_cost_vs$statusquo,

      #########
      # QALYs #
      #########

      cost.statusquo = map(interv_cost_vs, "statusquo"),
      cost.screened = map(interv_cost_vs, "screened"),

      E_QALY_screened = mean(interv_QALY_vs$screened),
      QALY.screened_person = interv_QALY_vs$screened/pop_year,
      QALY.statusquo_person = interv_QALY_vs$statusquo/pop_year,
      QALYgain = interv_QALY_vs$screened - interv_QALY_vs$statusquo)

  return(
    c(res,

      cost_incur_person = res$cost_incur/pop_year,
      E_cost_incur = mean(res$cost_incur),
      E_cost_incur_person = mean(res$cost_incur_person),

      QALYgain_person = res$QALYgain/pop_year,
      E_QALYgain = mean(res$QALY_gain),
      E_QALYgain_person = mean(res$QALYgain_person)
    ))
}
