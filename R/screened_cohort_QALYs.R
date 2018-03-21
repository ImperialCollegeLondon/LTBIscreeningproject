
#' Calculate Potentially Screened Cohort QALYs
#'
#' @param n.diseasefree Number of disease-free individuals
#' @param QALY List of QALYs for total cohort status-quo (assumed treated and cured),
#'             death, or all treated to disease-free
#'
#' @return Total QALYs for potentially screened cohort
#' @export
#' @seealso \code{\link{screened_cohort_cost}}
#'
#' @examples
#'
screened_cohort_QALYs <- function(n.diseasefree,
                                  QALY) {

  res <- QALY$cured
  n.tb <- length(res)

  if (length(n.diseasefree) > 0) {

    which_diseasefree <- sample(1:n.tb, n.diseasefree)
    res[which_diseasefree] <- QALY$diseasefree[which_diseasefree]
  }

  return(res)
}
