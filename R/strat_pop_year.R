
#' append_scenario_num
#'
#' @param dat
#' @param i
#' @param excluded_cols
#'
#' @return
#' @export
append_scenario_num <- function(dat,
                                i,
                                excluded_cols = c("year", "discount")) {

  scenario_cols <- !names(dat) %in% excluded_cols

  names(dat)[scenario_cols] <-
    paste0(names(dat)[scenario_cols], i)

  dat
}


#' strat_pop_year
#'
#'
#' @param cohort
#' @param dectree_res
#' @param prop_avoid
#' @param folders
#'
#' @return
#' @export
#'
#' @examples
strat_pop_year <- function(cohort,
                           dectree_res,
                           prop_avoid,
                           folders) {

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
  res <-
    strat_pop_year %>%
    data.frame(row.names = NULL) %>%
    mutate(discount = discount(t_limit = n()),
           c_tb = round(means$cost.aTB_TxDx + (means$num_sec_inf * means$cost.aTB_TxDx)/1.035, 2),
           c_screen = round(mean(dectree_res[[i]]$mc_cost), 2),
           totc_screen = screen * c_screen,
           totc_tb = tb * c_tb,
           dis_totc_tb = as.integer(discount * totc_tb),
           dis_totc_screen = as.integer(discount * totc_screen))

  return(res)
}
