
#' subpop_by_exituk_year
#'
#' count number deaths & active TB cases in each exit uk year group
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#'
subpop_by_exituk_year <- function(data) {


  strat_exit_year <- list()
  exit_max_year <- 10

  for (yeari in seq_len(exit_max_year)) {

    # single year cohort
    # exit in yeari and exit first event (before death, active tb, followup censoring)

    cohort_subset <-
      data %>%
      dplyr::filter((yeari - 1) < date_exit_uk1_issdt.years,
                    date_exit_uk1_issdt.years < yeari,
                    date_exit_uk1_issdt.years < date_death1_issdt.years,
                    uk_tb_orig == 0)

    strat_exit_year[[yeari]] <-
      list(tb = cohort_subset$exituk_tb_year,
           death = cohort_subset$date_death1_issdt.years) %>%
      count_comprsk_events()
  }

  return(strat_exit_year)
}
