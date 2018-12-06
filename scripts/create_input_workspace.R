
#' ---
#' title: "LTBI screening model:
#' generate a fixed synthetic cohort and
#' set constants; only need to run this once"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---


data("051206 - IMPUTED_sample")


source("scripts/data-prep_policies.R")

interv()

source("scripts/01b-data-prep_cost-effectiveness.R", echo = TRUE)

source("scripts/01-data-prep_scenario.R", echo = TRUE)

source("scripts/01c-data-prep_modelling.R", echo = TRUE)

source("scripts/04a_3-include-new-tb-events.R", echo = TRUE)


# source("data-raw/active-TB-extrapolation.R", echo = TRUE)
