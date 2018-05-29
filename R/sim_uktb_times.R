#'
#' #' Sample active TB Progression Time After Follow-up
#' #'
#' #' This approach samples active TB time once and if its
#' #' after the death date assumes no pogression.
#' #'
#' #' @param fup_issdt Time to follow-up/exit UK
#' #' @param death_issdt Time to all-cause death (competing risk)
#' #' @param prob Incidence density of progression
#' #'
#' #' @return Vector of times
#' #' @export
#' #'
#' #' @examples
#' #'
#' sample_tb_year2 <- function(fup_issdt,
#'                                 death_issdt,
#'                                 prob) {
#'
#'   disease_free_yrs <- 0:fup_issdt #+1?
#'
#'   prob_noevent <- 1 - sum(prob)
#'   prob[disease_free_yrs] <- 0
#'
#'   tb_year <- sample(x = c(seq_along(prob), Inf),
#'                     size = 1,
#'                     prob = c(prob, prob_noevent))
#'
#'   # competing risk all-cause mortality
#'   tb_year <- if_else(condition = tb_year > death_issdt,
#'                      true = Inf,
#'                      false = tb_year)
#'
#'   return(tb_year)
#' }
#'

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

  tb_year <- vector(length = pop,
                    mode = "double")

  mat <- cbind(fup_issdt = ceiling(data$fup_issdt),
               death_issdt = ceiling(data$date_death1_issdt.years),
               tb_issdt = ceiling(data$rNotificationDate_issdt.years),
               LTBI = as.logical(data$LTBI),
               exit_uk = as.logical(data$exit_uk1),
               death = as.logical(data$death1),
               uk_tb = as.logical(data$uk_tb))

  for (i in seq_len(pop)) {

    mati <- mat[i, ]

    tb_year[i] <-

      if (!mati['LTBI']) {

        Inf

      # if other event observed before end of follow-up
      }else if (mati['exit_uk'] || mati['death']) {

        NA

      }else if (mati['uk_tb']) {

        mati['tb_issdt']

      }else {

        sample_tb_year(mati['fup_issdt'],
                       mati['death_issdt'],
                       prob)
      }
  }

  return(tb_year)
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

  tb_year <- vector(length = pop,
                    mode = "double")

  mat <- cbind(fup_issdt = ceiling(data$date_exit_uk1_issdt.years),
               death_issdt = ceiling(data$date_death1_issdt.years),
               LTBI = as.logical(data$LTBI),
               exit_uk = as.logical(data$exit_uk1))

  for (i in seq_len(pop)) {

    mati <- mat[i, ]

    tb_year[i] <-

      if (!mati['LTBI']) {

        Inf

        # not first observed event
      }else if (!mati['exit_uk']) {

        NA

      }else {

        sample_tb_year(mati['fup_issdt'],
                       mati['death_issdt'],
                       prob)
      }
  }

  return(tb_year)
}

