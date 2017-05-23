
#' Simulate Active TB Progression Times for Exit UK Individuals
#'
#' Basic brute-force approach.
#'
#' @param data Times of events, LTBI status, event indicators
#' @param prob Probabilities of progressing to active TB each year
#'
#' @return
#' @export
#'
sim_aTB_times <- function(data,
                          prob) {

  pop <- nrow(data)

  exituk_tb_year <- vector(length = pop,
                           mode = "double")

  for (i in seq_len(pop)) {

    # LTBI-free
    if (data$LTBI[i] == 0) {

      exituk_tb_year[i] <- Inf

    # not first event
    }else if (!data$exit_uk1[i]) {

      exituk_tb_year[i] <- NA
    }else {

      prob_after_exituk <- prob

      disease_free_yrs <- 0:data$date_exit_uk1_issdt.years[i] + 1

      prob_after_exituk[disease_free_yrs] <- 0

      exituk_tb_year[i] <- sample(x = c(seq_along(prob_after_exituk), Inf),
                                  size = 1,
                                  prob = c(prob_after_exituk, 1 - sum(prob)))

      exituk_tb_year[i] <- if_else(condition = exituk_tb_year[i] > data$date_death1_issdt.years[i],
                                   true = Inf,
                                   false = exituk_tb_year[i])
    }
  }

  return(exituk_tb_year)
}
