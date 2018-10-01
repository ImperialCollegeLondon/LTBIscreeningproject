
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

append_scenario_num <- function(strat_pop_year, i) {

  scenario_cols <- !names(strat_pop_year) %in% c("year", "discount")

  names(strat_pop_year)[scenario_cols] <-
    paste0(names(strat_pop_year)[scenario_cols], i)

  strat_pop_year
}



##TODO:
# strat_pop_year <- function(cohort,
#                            dectree_res,
#                            folders) {
#
# }

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

  id_avoid <- cohort$id_avoided_tb
  who_avoided <- rows_first_n_ids(id_avoid,
                                  prop_avoid)

  cohort_screen <- cohort
  cohort_screen$notif_issdt.years[who_avoided] <- Inf

  event_times$tb = cohort_screen$notif_issdt.years

  strat_pop_year <- count_comprsk_events(event_times)

  strat_pop_year <- cbind.data.frame(strat_pop_year,
                                     screen = screen[1:nrow(strat_pop_year)])

  # append average costs and discounted costs

  strat_pop_year <-
    data.frame(strat_pop_year, row.names = NULL) %>%
    mutate(discount = discount(t_limit = n()),
           c_tb = round(means$cost.aTB_TxDx + (means$num_sec_inf * means$cost.aTB_TxDx)/1.035, 2),
           c_screen = round(mean(dectree_res[[i]]$mc_cost), 2),
           totc_screen = screen * c_screen,
           totc_tb = tb * c_tb,
           dis_totc_tb = as.integer(discount * totc_tb),
           dis_totc_screen = as.integer(discount * totc_screen))

  strat_pop_year <- append_scenario_num(strat_pop_year, i)

  res <- dplyr::full_join(res, strat_pop_year,
                          by = c("year", "discount"))
}

write.csv(res,
          file = pastef(folders$output$scenario, 'num-competing-events-by-year_screen.csv'))

