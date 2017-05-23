
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

    # LTBI-free
    if (data$LTBI[i] == 0) {

      uk_tb_year[i] <- Inf

    }else if (data$exit_uk1[i]) {

      uk_tb_year[i] <- NA

    }else if (as.logical(data$uk_tb[i])) {

      uk_tb_year[i] <- data$rNotificationDate_issdt.years[i]

    }else {

      prob_after_fup <- prob

      disease_free_yrs <- 0:data$fup_issdt[i]

      prob_after_fup[disease_free_yrs] <- 0

      uk_tb_year[i] <- sample(x = c(seq_along(prob_after_fup), Inf),
                              size = 1,
                              prob = c(prob_after_fup, 1 - sum(prob)))

      uk_tb_year[i] <- if_else(condition = uk_tb_year[i] > data$date_death1_issdt.years[i],
                               true = Inf,
                               false = uk_tb_year[i])
    }
  }

  return(uk_tb_year)
}
