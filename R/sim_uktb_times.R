
#' Sample active TB Progression Time After Follow-up
#'
#' This approach samples active TB time once and if its
#' after the death date assumes no pogression.
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
sample_tb_year2 <- function(fup_issdt,
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

#' Sample active TB Progression Time After Follow-up
#'
#' Given that an individual progresses then this approach
#' samples active TB times until one is before the death date.
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

  noevent <- sum(prob) < runif(1)

  disease_free_yrs <- 0:fup_issdt
  prob[disease_free_yrs] <- 0

  if (noevent) {

    return(tb_year)

  }else{
    i <- 1

    if (fup_issdt == ceiling(death_issdt)) {
      return(fup_issdt)
    }

    while (tb_year > ceiling(death_issdt)) {

      if (i > 100)

      tb_year <- sample(x = seq_along(prob),
                        size = 1,
                        prob = prob)
      i <- i + 1
    }

    return(tb_year)

  }
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
  pop <- nrow(data)

  uk_tb_year <- vector(length = pop,
                       mode = "double")

  for (i in seq_len(pop)) {

    uk_tb_year[i] <-

      # LTBI-free
      if (data$LTBI[i] == 0) {

        Inf

      # if other event before end of follow-up
      }else if (data$exit_uk1[i] | data$death1[i]) {

        NA

      }else if (as.logical(data$uk_tb[i])) {

        data$rNotificationDate_issdt.years[i]

      }else {

        sample_tb_year(data$fup_issdt[i],
                       data$date_death1_issdt.years[i],
                       prob)
      }
  }

  return(uk_tb_year)
}


#' Simulate Active TB Progression Times for Exit UK Individuals
#'
#' Basic brute-force approach.
#'
#' @param data Times of events, LTBI status, event indicators
#' @param prob Probabilities of active TB each year. Progression from LTBI needs rescaling.
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
                       prob/data$p_LTBI[i])
      }
  }

  return(exituk_tb_year)
}

