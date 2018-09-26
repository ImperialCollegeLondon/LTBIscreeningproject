
#' Count Competing Risks Events Over Time
#'
#' Provide vectors or event times, where an individual
#' can have multiple latent event times.
#' The earliest of these is used at the actual time.
#'
#' In cases where two events happen in the same time interval
#' the first event in the list argument is used.
#'
#' If this weren't the case then it would be easier to calculate.
#'
#' @param event_times List of individual-level time series
#' @param fup_max_year Time horizon
#' @param fill Append counts up to time horixon after risk-set empty
#'
#' @return Event counts by year (wide matrix). Note that the risk-set is for the following year (remaining)
#'         i.e. column year 1 is the risk set for year 2.
#' \itemize{
#'   \item year
#'   \item tb
#'   \item exit_uk
#'   \item death
#'   \item at-risk
#' }
#'
#' @export
#'
#' @examples
#'
#' attach(cohort)
#'
#' event_times <- list(tb = notif_issdt.years,
#'                          exit_uk = date_exit_uk1_issdt.years,
#'                          death = date_death1_issdt.years)
#'
#' strat_pop_year <- count_comprsk_events(event_times) %>% t()
#'
#' detach(cohort)
#'
count_comprsk_events <- function(event_times,
                                 fup_max_year = 100,
                                 fill = TRUE){

  if (missing(event_times)) {
    stop("Require event times")
  }

  n_events <- lapply(event_times, length) %>%
    unlist()

  if (!assert_all_equal(n_events)) {
    stop("Require number of time points to be the same for all events")
  }

  if (!is.list(event_times)) {
    stop("Require list for event_times")
  }

  # replace missing event with after time horizon
  event_times <- lapply(event_times,
                        function(x) replace(x, is.na(x), 1001))

  N_pop <- n_events[1]

  event_counts <- NULL

  for (yeari in seq_len(fup_max_year)) {

    # FALSE means removed from the population
    risk_set <- rep(TRUE, N_pop)
    num_event_year <- NULL

    for (j in seq_along(event_times)) {

      # at-risk pop who experiences eventi before yeari
      event_true <- event_times[[j]][risk_set] < yeari

      risk_set <- remove_from_riskset(risk_set, event_true)

      num_new_events <- sum(event_true)
      num_event_year <- c(num_event_year,
                          num_new_events)
    }

    event_counts <- cbind(event_counts,
                          c(yeari,
                            num_event_year,
                            N_pop - sum(num_event_year)))

    # risk-set = 0
    if (N_pop - sum(num_event_year) <= 0) break
  }

  event_counts <- fill_to_fup_max_year(event_counts,
                                       fup_max_year)

  rownames(event_counts) <- c("year",
                              names(event_times),
                              "at-risk")

  event_counts['year', ] <- seq_len(fup_max_year)

  return(event_counts)
}


# fill by last value carried forward
#'
#' @param event_counts vector or TRUE FALSE
#' @param fup_max_year time horizon
#'
#' @return
#' @export
fill_to_fup_max_year <- function(event_counts,
                                 fup_max_year) {

  num_years_events <- ncol(event_counts)

  missing_years <- matrix(event_counts[, num_years_events, drop = FALSE],
                          byrow = FALSE,
                          nrow = nrow(event_counts),
                          ncol = fup_max_year - num_years_events)

  cbind(event_counts, missing_years)
}


#' remove_from_riskset
#'
#' @param risk_set vector or TRUE FALSE
#' @param event_true vector or TRUE FALSE
#'
#' @return
#' @export
remove_from_riskset <- function(risk_set,
                                event_true) {

  risk_set[risk_set][event_true] <- FALSE
  risk_set
}


#
assert_all_equal <- function(x) {

  abs(max(x) - min(x)) == 0
}
