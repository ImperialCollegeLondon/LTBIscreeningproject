context("test-scenario_cost.R")

library(assertthat)


load("costeff_cohort.RData")

test_that("struct", {

  res <- scenario_cost(endpoint = "death",
                       unit_cost.aTB_TxDx = list(distn = "unif", params = c(min = 1, max = 1)),
                       num_2nd_inf = list(distn = "unif", params = c(min = 1, max = 1)),
                       avoid_tb = c("death" = 10, "exit uk" = 1),
                       costeff_cohort = costeff_cohort)

  expect_is(res, "list")
  expect_length(res, 2)
  expect_named(res, c("statusquo", "screened"))
})

test_that("edge cases", {

  res <- scenario_cost(endpoint = "death",
                       unit_cost.aTB_TxDx = list(distn = "unif", params = c(min = 1, max = 1)),
                       num_2nd_inf = list(distn = "unif", params = c(min = 1, max = 1)),
                       avoid_tb = c("death" = 0, "exit uk" = 10),
                       costeff_cohort = costeff_cohort)

  expect_equal(res[[1]] - res[[2]], 0)

  res_uk <- scenario_cost(endpoint = "exit uk",
                       unit_cost.aTB_TxDx = list(distn = "unif", params = c(min = 1, max = 1)),
                       num_2nd_inf = list(distn = "unif", params = c(min = 1, max = 1)),
                       avoid_tb = c("death" = 10, "exit uk" = 0),
                       costeff_cohort = costeff_cohort)

  expect_equal(res_uk[[1]] - res_uk[[2]], 0)

  expect_gte(res[[1]] - res_uk[[1]], 0)
})


test_that("errors and warnings", {

  expect_error(scenario_cost(endpoint = "death",
                             unit_cost.aTB_TxDx = 1,
                             num_2nd_inf = list(distn = "unif", params = c(min = 1, max = 1)),
                             avoid_tb = c("death" = 10, "exit uk" = 1),
                             costeff_cohort = costeff_cohort))

  expect_error(scenario_cost(endpoint = "death",
                             unit_cost.aTB_TxDx = list(distn = "unif", params = c(min = 1, max = 1)),
                             num_2nd_inf = 1,
                             avoid_tb = c("death" = 10, "exit uk" = 1),
                             costeff_cohort = costeff_cohort))

})
