
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


library(LTBIscreeningproject)
library(dplyr)
library(reshape2)
library(tibble)

load(here::here("data", "model_input_cohort.RData"))

attach(IMPUTED_sample)

event_times <-
  list(
    tb = all_tb_issdt,
    # tb = notif_issdt.years,
    exit_uk = date_exit_uk1_issdt.years,
    death = date_death1_issdt.years)

detach(IMPUTED_sample)

lapply(event_times, head, 20)

strat_pop_year <-
  event_times %>%
  count_comprsk_events() #%>%
  #as.tibble()

head(strat_pop_year)

plot(strat_pop_year$tb, type = 'o',
     xlab = "Years since migration", ylab = "Cases of TB")#,
     # xlim = c(0,100), ylim = c(0,400))

plot(strat_pop_year$incid_rate, type = 'o',
     xlab = "Years since migration", ylab = "Cases of TB per 100,000 person-years")#,
     # xlim = c(0,100), ylim = c(0,300))

