
#' Calculate Potentially Screened Cohort Costs
#'
#' Substract the avoided cost of those successfully screened
#' from status-quo cost.
#'
#' @param n.diseasefree Number of disease-free individuals
#' @param cost.statusquo Cost under status-quo
#' @param unit_cost_case Unit cost of detect and treat an active TB case
#'
#' @return Total cost for potentially screened cohort
#' @export
#' @seealso \code{\link{screened_cohort_QALYs}}
#'
#' @examples
#'
screened_cohort_cost <- function(n.diseasefree,
                                 cost.statusquo,
                                 unit_cost_case) {

  if (length(n.diseasefree) == 0) {
    n.diseasefree <- 0
  }

  return(cost.statusquo - (unit_cost_case * n.diseasefree))
}
