
#' Calculate Jump Probabilities from Cumulative Incidence Functions
#'
#' see Jackson ()
#'
#' @param cum_incidence_event Cumulative incidence for the event of interest
#' @param cum_incidence_comprisks List of cumulative incidence for the other competing risk events
#'
#' @return Discrete probabilities
#' @export
#'
#' @examples
#'
prob_from_cum_incidence <- function(cum_incidence_event,
                                    cum_incidence_comprisks){

  comprisks_length <- lapply(cum_incidence_comprisks, FUN = length)

  if (!all(comprisks_length == length(cum_incidence_event))) {
    stop("cumulative incidence lengths different")}

  # sum all CIF for 1 - overall survival
  F_t <- Reduce("+", cum_incidence_comprisks) + cum_incidence_event
  F_t <- trim_last(F_t)

  return(diff(cum_incidence_event)/(1 - F_t))
}
