
#' Count Competing Risks Events Over Time
#'
#' Provide vectors or event times, where an individual
#' can have multiple latent event times.
#' The earliest of these is used at the actual time.
#' In cases where two events happen in the same time interval
#' the first event in the list argument is used.
#'
#' @param indiv_event_times List of individual-level time series
#' @param followup_max_year Time horizon
#'
#' @return Event counts by year
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
                                 followup_max_year = 100){

  if(missing(indiv_event_times)){
    stop("Require event times")
  }

  n_events <- lapply(indiv_event_times, length) %>%
                unlist()

  if(!isTRUE(all.equal(abs(max(n_events) - min(n_events)), 0))){
    stop("Require number of time points to be the same for all events")
  }

  if(!is.list(indiv_event_times)){
    stop("Require list for indiv_event_times")
  }

  # replace missing event with after time horizon
  indiv_event_times <- lapply(indiv_event_times,
                              function(x) replace(x, is.na(x), 101))

  N_pop <- length(indiv_event_times[[1]])

  event_counts <- NULL

  for (yeari in seq_len(followup_max_year)){

    # FALSE means removed from the population
    risk_set <- rep(TRUE, N_pop)
    event_year <- NULL

    for (eventi in seq_along(indiv_event_times)){

      event_lgl <- indiv_event_times[[eventi]][risk_set]<yeari
      risk_set[risk_set][event_lgl] <- FALSE
      event_year <- c(event_year, sum(event_lgl))
    }

    event_counts <- cbind(event_counts, c(yeari,
                                          event_year,
                                          N_pop - sum(event_year)))
  }

  rownames(event_counts) <- c("year",
                              names(indiv_event_times),
                              "remainder")
  return(event_counts)
}
