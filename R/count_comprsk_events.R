
#' Count Competing Risks Events Over Time
#'
#' Provide vectors or event times, where an individual
#' can have multiple latent event times.
#' The earliest of these is used at the actual time.
#' In cases where two events happen in the same time interval
#' the first event in the list argument is used.
#'
#' If this we're the case then it would be easier to calculate.
#'
#' @param indiv_event_times List of individual-level time series
#' @param followup_max_year Time horizon
#' @param fill Append counts up to time horixon after risk-set empty
#'
#' @return Event counts by year. Note that the risk-set is for the following year i.e. column year 1 is the risk set for year 2.
#' @export
#'
#' @examples
#'
#' attach(IMPUTED_sample_year_cohort)
#'
#' strat_pop_year <- list(tb = rNotificationDate_issdt.years,
#'                        exit_uk = date_exit_uk1_issdt.years,
#'                        death = date_death1_issdt.years) %>%
#'                        count_comprsk_events()
#'
#' detach(IMPUTED_sample_year_cohort)
#'
count_comprsk_events <- function(indiv_event_times,
                                 followup_max_year = 100,
                                 fill = TRUE){

  if (missing(indiv_event_times)) {
    stop("Require event times")
  }

  n_events <- lapply(indiv_event_times, length) %>%
                unlist()

  if (!isTRUE(all.equal(abs(max(n_events) - min(n_events)), 0))) {
    stop("Require number of time points to be the same for all events")
  }

  if (!is.list(indiv_event_times)) {
    stop("Require list for indiv_event_times")
  }

  # replace missing event with after time horizon
  indiv_event_times <- lapply(indiv_event_times,
                              function(x) replace(x, is.na(x), 1001))

  N_pop <- n_events[1]

  event_counts <- NULL

  for (yeari in seq_len(followup_max_year)) {

    # FALSE means removed from the population
    risk_set <- rep(TRUE, N_pop)
    num_event_year <- NULL

    for (eventi in seq_along(indiv_event_times)) {

      # of the at-risk pop who experiences eventi before yeari
      event_lgl <- indiv_event_times[[eventi]][risk_set] < yeari

      # remove recent event indiv from risk set
      risk_set[risk_set][event_lgl] <- FALSE

      num_new_events <- sum(event_lgl)
      num_event_year <- c(num_event_year,
                          num_new_events)
    }

    event_counts <- cbind(event_counts, c(yeari,
                                          num_event_year,
                                          N_pop - sum(num_event_year)))

    # risk-set = 0
    if (N_pop - sum(num_event_year) <= 0) break
  }

  # fill to followup_max_year
  # last value carried forward
  num_years_events <- ncol(event_counts)

  missing_years <- matrix(event_counts[, num_years_events, drop = FALSE],
                          byrow = FALSE,
                          nrow = nrow(event_counts),
                          ncol = followup_max_year - num_years_events)

    event_counts <- cbind(event_counts, missing_years)


  rownames(event_counts) <- c("year",
                              names(indiv_event_times),
                              "at-risk")
  return(event_counts)
}
