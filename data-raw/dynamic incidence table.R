
#' ---
#' title: "LTBI screening model:
#' Dynamic incidence table"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---

#1. Vandenbroucke, J. P. & Pearce, N. Incidence rates in dynamic populations. Int. J. Epidemiol. 41, 1472–1479 (2012).

attach(IMPUTED_sample)

event_times <-
  list(
    # tb = IMPUTED_sample$iss,
    tb = notif_issdt.years,
    exit_uk = date_exit_uk1_issdt.years,
    death = date_death1_issdt.years)

detach(IMPUTED_sample)


strat_pop_year <-
  event_times %>%
  count_comprsk_events() %>%
  t() %>%
  as.tibble()

strat_pop_year$atrisk_start <- c(nrow(IMPUTED_sample), strat_pop_year$'at-risk')[1:(nrow(strat_pop_year))]

strat_pop_year$mean_atrisk <- floor(strat_pop_year$'atrisk_start' + (strat_pop_year$'atrisk_start' - strat_pop_year$'at-risk')/2)

strat_pop_year$tb_diff <- c(0, diff(strat_pop_year$tb))

strat_pop_year$incid_rate <- strat_pop_year$tb_diff/strat_pop_year$mean_atrisk*100000

plot(strat_pop_year$tb_diff, type = 'o',
     xlab = "Year", ylab = "Cases of TB per 100,000 person-years")

plot(strat_pop_year$incid_rate, type = 'o',
     xlab = "Year", ylab = "Cases of TB per 100,000 person-years")
