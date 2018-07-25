context("test-scenario_cost.R")

library(assertthat)
library(treeSimR)


load("costeff_cohort.RData")

costeff_cohort$num_contacts <- 1
costeff_cohort$all_tb <- TRUE
costeff_cohort$uk_tb <- c(TRUE, FALSE)
costeff_cohort$id_avoided_tb <- 1:nrow(costeff_cohort)


test_that("struct", {

  res <-
    scenario_cost(
      endpoint = "death",
      unit_cost = list(TST =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_Dx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_Tx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       LTBI_DxTx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_TxDx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1))),
      probs_contact = c(contact = 1,
                        aTB_Dx = 0.1 + 0.018,
                        aTB_Tx = 0.018,
                        LTBI_DxTx = 0.1),
      costeff_cohort = costeff_cohort,
      prop_avoided = 0.5
    )

  expect_is(res, "list")
  expect_length(res, 2)
  expect_named(res, c("statusquo", "screened"))
})


test_that("edge cases", {

  res <-
    scenario_cost(
      endpoint = "death",
      unit_cost = list(TST =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_Dx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_Tx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       LTBI_DxTx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_TxDx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1))),
      probs_contact = c(contact = 1,
                        aTB_Dx = 0.1 + 0.018,
                        aTB_Tx = 0.018,
                        LTBI_DxTx = 0.1),
      costeff_cohort = costeff_cohort,
      prop_avoided = 0
    )

  res_uk <-
    scenario_cost(
      endpoint = "exit uk",
      unit_cost = list(TST =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_Dx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_Tx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       LTBI_DxTx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_TxDx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1))),
      probs_contact = c(contact = 1,
                        aTB_Dx = 0.1 + 0.018,
                        aTB_Tx = 0.018,
                        LTBI_DxTx = 0.1),
      costeff_cohort = costeff_cohort,
      prop_avoided = 0
    )

  expect_equal(res[[1]], res[[2]])
  expect_equal(res_uk[[1]], res_uk[[2]])

})



test_that("uk vs all cases", {

  res <-
    scenario_cost(
      endpoint = "death",
      unit_cost = list(TST =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_Dx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_Tx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       LTBI_DxTx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_TxDx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1))),
      probs_contact = c(contact = 1,
                        aTB_Dx = 0.1 + 0.018,
                        aTB_Tx = 0.018,
                        LTBI_DxTx = 0.1),
      costeff_cohort = costeff_cohort,
      prop_avoided = 0.5
    )

  res_uk <-
    scenario_cost(
      endpoint = "exit uk",
      unit_cost = list(TST =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_Dx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_Tx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       LTBI_DxTx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_TxDx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1))),
      probs_contact = c(contact = 1,
                        aTB_Dx = 0.1 + 0.018,
                        aTB_Tx = 0.018,
                        LTBI_DxTx = 0.1),
      costeff_cohort = costeff_cohort,
      prop_avoided = 0.5
    )

  expect_gte(res[[1]], res[[2]])
  expect_gte(res_uk[[1]], res_uk[[2]])
  expect_gte(res[[1]], res_uk[[1]])
})


test_that("prop_avoided", {

  res_05 <-
    scenario_cost(
      endpoint = "death",
      unit_cost = list(TST =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_Dx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_Tx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       LTBI_DxTx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_TxDx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1))),
      probs_contact = c(contact = 1,
                        aTB_Dx = 0.1 + 0.018,
                        aTB_Tx = 0.018,
                        LTBI_DxTx = 0.1),
      costeff_cohort = costeff_cohort,
      prop_avoided = 0.5
    )

  res_1 <-
    scenario_cost(
      endpoint = "death",
      unit_cost = list(TST =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_Dx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_Tx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       LTBI_DxTx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1)),
                       aTB_TxDx =
                         list(distn = "unif",
                              params = c(min = 1, max = 1))),
      probs_contact = c(contact = 1,
                        aTB_Dx = 0.1 + 0.018,
                        aTB_Tx = 0.018,
                        LTBI_DxTx = 0.1),
      costeff_cohort = costeff_cohort,
      prop_avoided = 1
    )

  expect_equal(res_1$screened, 0)
  expect_equal(res_1$statusquo, res_05$statusquo)
  expect_gt(res_05$screened, res_1$screened)

})


test_that("errors and warnings", {

  # expect_error(
  #   scenario_cost(
  #     endpoint = "death",
  #     unit_cost = 1,
  #     prop_avoided = 0,
  #     costeff_cohort = costeff_cohort),
  #   regexp = "Distributions not specified in a list."
  # )

})
