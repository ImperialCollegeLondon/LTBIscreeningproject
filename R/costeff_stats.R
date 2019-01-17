
#' Cost-effectiveness Statistics
#'
#' For a scenario the population model with active TB cases.
#'
#' @param scenario_dat list
#' @param interv_QALY list of \code{scenario_QALY()} output
#' @param interv_cost list of \code{scenario_cost()} output
#' @param pop_year integer
#'
#' @return list
#' \itemize{
#'    \item QALY.statusquo
#'    \item QALY.screened
#'    \item E_cost_screened: mean average
#'    \item cost.screened_person
#'    \item cost.statusquo_person
#'    \item cost_incur
#'    \item cost.statusquo
#'    \item cost.screened
#'    \item E_QALY_screened: mean average
#'    \item QALY.screened_person
#'    \item QALY.statusquo_person
#'    \item QALYgain
#'    \item cost_incur_person
#'    \item E_cost_incur: mean average
#'    \item E_cost_incur_person: mean average
#'    \item QALYgain_person
#'    \item E_QALYgain: mean average
#'    \item E_QALYgain_person: mean average
#' }
#'
#' @export
#'
#' @examples
#'
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

      QALY.statusquo = interv_QALY_vs$statusquo,
      QALY.screened = interv_QALY_vs$screened,

      E_cost_screened = mean(interv_cost_vs$screened),
      cost.screened_person = interv_cost_vs$screened/pop_year,
      cost.statusquo_person = interv_cost_vs$statusquo/pop_year,
      cost_incur = interv_cost_vs$screened - interv_cost_vs$statusquo,

      #########
      # QALYs #
      #########

      cost.statusquo = interv_cost_vs$statusquo,
      cost.screened = interv_cost_vs$screened,

      E_QALY_screened = mean(interv_QALY_vs$screened),
      QALY.screened_person = interv_QALY_vs$screened/pop_year,
      QALY.statusquo_person = interv_QALY_vs$statusquo/pop_year,
      QALYgain = interv_QALY_vs$screened - interv_QALY_vs$statusquo) %>%

    update_list(
      cost_incur_person = ~cost_incur/pop_year,
      E_cost_incur = ~mean(cost_incur),
      E_cost_incur_person = ~mean(cost_incur/pop_year),

      QALYgain_person = ~QALYgain/pop_year,
      E_QALYgain = ~mean(QALYgain),
      E_QALYgain_person = ~mean(QALYgain/pop_year))

  return(res)
}
