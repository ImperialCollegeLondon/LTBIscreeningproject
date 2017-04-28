
#' Simulate Active TB Progression Times for Exit UK Individuals
#'
#' Basic brute-force approach.
#' Sample active TB status each year using incidence probabilities.
#' Take first instance after exit uk.
#'
#' @param pop
#' @param data
#' @param prob
#'
#' @return
#' @export
#'
sim_aTB_times <- function(pop,
                          data,
                          prob) {

  exituk_tb_year <- vector(length = pop,
                           mode = "double")

  for (i in seq_len(pop)) {

    # LTBI free
    if (data$LTBI[i] == 0) {

      exituk_tb_year[i] <- Inf
    }else if (!data$exit_uk1[i]) {

      exituk_tb_year[i] <- NA
    }else {

      prob_after_exituk <- prob

      prob_after_exituk[0:data$date_exit_uk1_issdt.years[i] + 1] <- 0

      exituk_tb_year[i] <- sample(x = seq_along(prob_after_exituk),
                                  size = 1,
                                  prob = prob_after_exituk)
    }
  }

  return(exituk_tb_year)
}
