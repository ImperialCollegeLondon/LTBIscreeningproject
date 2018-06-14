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

  tb_year <- Inf

  disease_free_yrs <- 0:fup_issdt
  prob[disease_free_yrs] <- 0

  noevent <- sum(prob) < runif(1)

  if (noevent) {

    return(tb_year)

  }else{
    i <- 1

    if (fup_issdt == death_issdt) {
      return(fup_issdt)
    }

    while (tb_year > death_issdt) {

      if (i %% 100 == 0) message("taking a long time to sample a TB event time")

      tb_year <- sample(x = seq_along(prob),
                        size = 1,
                        prob = prob)
      i <- i + 1
    }

    return(tb_year)
  }
}
