
#' ---
#' title: "LTBI screening model:
#' gridded plots of the subpopulation sizes over time
#' can do this on the raw, fitted, extrapolated or subsample data"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---

# tb is for cases in EWNI only


event_times <- list(tb = cohort$notif_issdt.years,
                    exit_uk = cohort$date_exit_uk1_issdt.years,
                    death = cohort$date_death1_issdt.years)

# append screening subpop
screen <- table(ceiling(cohort$screen_year))
screen <- c(screen, rep(0, 100 - length(screen)))

res <- data.frame(year = 1:100,
                  discount = discount(t_limit = 100))

n.scenarios <-  length(dectree_res)

for (i in seq_len(n.scenarios)) {

  # (average) screened subpop counts
  prop_avoid <- mean(dectree_res[[i]]$subset_pop[ ,'p_LTBI_to_cured'])

  strat_pop_year <- strat_pop_year(cohort,
                                   dectree_res,
                                   prop_avoid,
                                   folders)

  strat_pop_year <- append_scenario_num(strat_pop_year, i)

  res <- dplyr::full_join(res, strat_pop_year,
                          by = c("year", "discount"))
}

write.csv(res,
          file = pastef(folders$output$scenario, 'num-competing-events-by-year_screen.csv'))

