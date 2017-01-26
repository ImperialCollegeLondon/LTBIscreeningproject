
#' Calculate Potentially Screened Cohort QALYs
#'
#' @param n.diseasefree Number of disease-free individuals
#' @param QALY List of QALYs for total cohort status-quo (assumed treated and cured),
#'             death, or all treated to disease-free
#'
#' @return Total QALYs for potentially screened cohort
#' @export
#' @seealso \link{\code{create_screened_cohort_cost}}
#'
#' @examples
#'
create_screened_cohort_QALYs <- function(n.diseasefree,
                                         QALY) {

  totalQALY <-  QALY$cured
  n.tb <- length(totalQALY)

  if (length(n.diseasefree)>0) {

    which_diseasefree <- sample(1:n.tb, n.diseasefree)
    totalQALY[which_diseasefree] <- QALY$diseasefree[which_diseasefree]
  }

  return(sum(totalQALY))
}


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
#' @seealso \link{\code{create_screened_cohort_QALYs}}
#'
#' @examples
#'
create_screened_cohort_cost <- function(n.diseasefree,
                                        cost.statusquo,
                                        unit_cost_case){
  if (length(n.diseasefree)==0){
    n.diseasefree <- 0
  }

  return(cost.statusquo - (unit_cost_case * n.diseasefree))
}






