
#' Calculate Potentially Screened Cohort QALYs
#'
#' @param n.diseasefree Number of disease-free individuals
#' @param QALY_uk_tb List of QALYs for total cohort status-quo (assumed treated and cured), death, or all treated to disease-free
#'
#' @return sum total of QALYs for potentially screened cohort
#' @export
#'
#' @examples
#'
create_screened_cohort_QALYs <- function(n.diseasefree,
                                         QALY_uk_tb) {

  totalQALY.screened <-  QALY_uk_tb$cured
  n.tb_year <- length(totalQALY.screened)

  if (length(n.diseasefree)>0) {

    which_diseasefree <- sample(1:n.tb_year, n.diseasefree)
    totalQALY.screened[which_diseasefree] <- QALY_uk_tb$diseasefree[which_diseasefree]
  }

  return(sum(totalQALY.screened))
}


#' Calculate Potentially Screened Cohort Costs
#'
#' @param n.diseasefree
#' @param cost.statusquo
#' @param unit_case_cost
#'
#' @return sum total of cost for potentially screened cohort
#' @export
#'
#' @examples
#'
create_screened_cohort_cost <- function(n.diseasefree,
                                        cost.statusquo,
                                        unit_case_cost){
  if(length(n.diseasefree)==0){
    n.diseasefree <- 0
  }

  return(cost.statusquo - (unit_case_cost * n.diseasefree))
}
