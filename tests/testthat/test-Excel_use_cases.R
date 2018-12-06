context("Excel use cases validation")

## spreadsheet model in:
# C:\Users\ngreen1\Google Drive\LTBI-screening-cost-effectiveness\spreadsheet model\LTBI_screening_costs_calc - tests.xlsx

library(LTBIscreeningproject)
library(here)
library(miscUtilities)
library(dplyr)
library(parallel)
library(assertr); library(assertthat)
library(tibble)
library(magrittr)


# use case scenarios definitions:
file_tag <- "_Excel_test"
create_and_save_scenarios(file_tag)

## select cohort_single from:
load(here::here("data/single_migrant_cohort_Excel_test_cases.RData"))
cohort <- cohort_single[[1]]

interv_constructor(N.mc = 1,
                   use_discount = FALSE)

# set active tb cost as constant
data(unit_costs)
unit_cost$aTB_TxDx <- NULL
unit_cost$aTB_TxDx$distn = "none"
unit_cost$aTB_TxDx$params = c("mean" = 5410)
save(unit_cost,
     file = here::here("data", "unit_costs.RData"))


policy <<- 1

run_policy(cohort,
           make_plots = FALSE)

## load data

# spreadsheet model output:
excel_data <- read.csv(file = here::here("tests", "testthat", "excel_use_cases_output.csv"))

# select specific cohort
excel_data <-
  dplyr::filter(excel_data,
                cohort == 1) %>%
  arrange(scenario)

# R model output:
load(here::here("ext-data", "18_to_35_in_2009", "policy_001", "dectree_res.RData"))
load(here::here("ext-data", "18_to_35_in_2009", "policy_001", "popmod_res.RData"))


test_that("Excel use cases scenarios", {

    expect_equivalent(
      unlist(popmod_res$cost_incur, use.names = FALSE),
      excel_data$tb_cost)

    expect_equivalent(
      unlist(popmod_res$QALYgain, use.names = FALSE),
      excel_data$tb_QALYgain)

    expect_equivalent(
      map_dbl(dectree_res, "mc_cost"),
      excel_data$LTBI_cost)

    expect_equivalent(
      map_dbl(dectree_res, "mc_health"),
      excel_data$LTBI_QALYgain)

})

# load(here::here("ext-data", "18_to_35_in_2009", "policy_001", "ce_res.RData"))
