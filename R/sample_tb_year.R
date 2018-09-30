
#' Sample active TB progression time
#'
#' Given that an individual progresses then this approach
#' samples active TB times.
#'
#' Two-step mixture model for tb sampling:
#'
#' \enumerate{
#'   \item Do they progress?
#'   \item Sample TB time after follow-up
#' }
#'
#' @param fup_issdt Time to follow-up/exit EWNI
#' @param death_issdt Time to all-cause death (competing risk)
#' @param prob Incidence density of progression
#'
#' @return Notification time
#' @export
#'
#' @examples
#'
sample_tb_year <- function(fup_issdt,
                           death_issdt,
                           prob) {

  # competing risk
  prob[death_issdt:length(prob)] <- 0
  noevent <- sum(prob) < runif(1)

  # left truncation
  prob[0:fup_issdt] <- 0

  zero_probs <- sum(prob) == 0

  if (noevent || zero_probs) return(Inf)

  tb_year <- sample(x = seq_along(prob),
                    size = 1,
                    prob = prob)

  return(tb_year)
}
