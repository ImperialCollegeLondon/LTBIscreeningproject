
#' Sample (Updated) Active TB Status for Active TB Cases
#'
#' @param prob probability of success (e.g. completing treatment) for all cohort
#' @param is.tb is individual active TB case (logical)
#'
#' @return (counterfactual) TB status for active TB cases
#' @export
#'
#' @examples
#'
sample_uk_tb <- function(prob,
                         is.tb = NA){

  if (any(is.na(is.tb))) is.tb <- rep(TRUE, length(prob))

  n.tb <- sum(is.tb)

  as.numeric(prob[is.tb] < runif(n.tb))
}
