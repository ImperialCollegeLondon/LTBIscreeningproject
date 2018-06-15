#' Sample active TB progression time after right censoring
#'
#' Given that an individual progresses then this approach
#' samples active TB times until one is before the death date.
#'
#' Two-step mixture model for tb sampling:
#'   1. Do they progress?
#'   2. Sample TB time
#'
#' @param fup_issdt Time to follow-up/exit UK
#' @param death_issdt Time to all-cause death (competing risk)
#' @param prob Incidence density of progression
#'
#' @return Vector of times
#' @export
#'
#' @examples
#'
sample_tb_year <- function(fup_issdt,
                           death_issdt,
                           prob) {

  disease_free_yrs <- 0:fup_issdt
  prob[disease_free_yrs] <- 0

  noevent <- sum(prob) < runif(1)
  early_death <- fup_issdt >= death_issdt

  if (noevent | early_death) {

    return(Inf)

  }else{

    prob[death_issdt:length(prob)] <- 0

    tb_year <- sample(x = seq_along(prob),
                      size = 1,
                      prob = prob)
  }

  return(tb_year)
}
