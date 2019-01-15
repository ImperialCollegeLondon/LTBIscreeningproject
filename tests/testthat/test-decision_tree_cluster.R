context("decision tree cluster")


library(treeSimR)
library(data.tree)
library(purrr)
library(dplyr)

data("scenario_params")

test_that("basic structure: single scenario", {

  res <-
    decision_tree_cluster(params = scenario_params[[1]],
                          N.mc = 2,
                          cost_dectree = "osNode_cost_2009.Rds",
                          health_dectree = "osNode_cost_2009.Rds")

  expect_type(res, "list")
  expect_is(res$call, "call")
  expect_is(res$subset_pop, "matrix")
  expect_is(res$N.mc, "numeric")

  expect_length(res, 7)
  expect_named(res, c("mc_cost", "mc_health", "subset_pop", "osNode.cost", "osNode.health", "call", "N.mc"))

  expect_length(res$mc_cost, 2)
  expect_length(res$mc_health, 2)
  expect_length(res$subset_pop, 24)

  expect_equal(res$N.mc, 2)

})


test_that("", {

  res <- lapply(scenario_params[1:3],
                decision_tree_cluster,
                N.mc = 2)

  expect_type(res, "list")
  expect_length(res, 3)

})
