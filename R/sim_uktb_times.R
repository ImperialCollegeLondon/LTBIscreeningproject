
#' Sample active TB Progression Time After Follow-up
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

  disease_free_yrs <- 0:fup_issdt #+1?

  prob_noevent <- 1 - sum(prob)
  prob[disease_free_yrs] <- 0

  tb_year <- sample(x = c(seq_along(prob), Inf),
                    size = 1,
                    prob = c(prob, prob_noevent))

  # competing risk all-cause mortality
  tb_year <- if_else(condition = tb_year > death_issdt,
                     true = Inf,
                     false = tb_year)
  return(tb_year)
}


#' Simulate (Unobserved) Active TB Progression Times for UK Individuals
#'
#' Basic brute-force approach.
#'
#' @param data Times of events, LTBI status, event indicators
#' @param prob Probabilities of progressing to active TB each year
#'
#' @return Times from UK entry to TB notification
#' @export
#'
sim_uktb_times <- function(data,
                           prob) {

  pop <-  nrow(data)

  uk_tb_year <- vector(length = pop,
                       mode = "double")

  for (i in seq_len(pop)) {

    uk_tb_year[i] <-

      # LTBI-free
      if (data$LTBI[i] == 0) {

        Inf

      }else if (data$exit_uk1[i]) {

        NA

      }else if (data$death1[i]) {

        NA

      }else if (as.logical(data$uk_tb[i])) {

        data$rNotificationDate_issdt.years[i]

      }else if (data$cens1[i]) {

        sample_tb_year(data$fup_issdt[i],
                       data$date_death1_issdt.years[i],
                       prob)
      }else{
        stop("Don't know what's first event type.")
      }
  }

  return(uk_tb_year)
}


#' Simulate Active TB Progression Times for Exit UK Individuals
#'
#' Basic brute-force approach.
#'
#' @param data Times of events, LTBI status, event indicators
#' @param prob Probabilities of progressing to active TB each year
#'
#' @return Times from UK entry to TB notification
#' @export
#'
sim_exituk_tb_times <- function(data,
                                prob) {

  pop <- nrow(data)

  exituk_tb_year <- vector(length = pop,
                           mode = "double")

  for (i in seq_len(pop)) {

    exituk_tb_year[i] <-

      # LTBI-free
      if (data$LTBI[i] == 0) {

        Inf

      # not first event
      }else if (!data$exit_uk1[i]) {

        NA

      }else {

        sample_tb_year(data$date_exit_uk1_issdt.years[i],
                       data$date_death1_issdt.years[i],
                       prob)
      }
  }

  return(exituk_tb_year)
}

