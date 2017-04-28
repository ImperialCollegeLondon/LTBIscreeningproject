
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

  sample_tb <- function(p) {
    sample(c("tb", "disease-free"),
           size = 1,
           prob = c(p, 1 - p))}

  exituk_tb_year <- vector(length = pop,
                           mode = "double")

  for (i in seq_len(pop)) {

    # LTBI free
    if (data$LTBI[i] == 0) {

      exituk_tb_year[i] <- Inf
    }else{

      # sample if active TB each year
      # from incidence probs
      tb_year <-
        sapply(X = prob,
               FUN = sample_tb) %>%
        equals("tb") %>%
        which()

      # remove time if progress before exit uk
      # this captures never exit indivs
      tb_year[tb_year < data$date_exit_uk1_issdt.years[i]] <- NA

      # if multiple take first occurence
      exituk_tb_year[i] <-
        min(tb_year, na.rm = TRUE) %>%
        suppressWarnings()
    }
  }

  return(exituk_tb_year)
}
