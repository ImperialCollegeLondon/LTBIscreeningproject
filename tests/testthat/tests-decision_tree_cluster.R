context("decision tree cluster")


library(treeSimR)
library(data.tree)
library(purrr)

data("scenario_parameters")

test_that("basic structure", {

  res <- decision_tree_cluster(params = scenario_parameters[[1]],
                               N.mc = 2,
                               cost_dectree = "osNode_cost_2009.Rds",
                               health_dectree = "osNode_cost_2009.Rds")

  expect_named(res, c("mc_cost", "mc_health", "subset_pop", "osNode.cost", "osNode.health", "call", "N.mc"))

  expect_length(res, 7)
  expect_length(res$mc_cost, 2)
  expect_length(res$mc_health, 2)

  expect_equal(res$N.mc, 2)

  expect_is(res, "list")
  expect_is(res$call, "call")

})



# res <- lapply(scenario_parameters,
#               decision_tree_cluster,
#               N.mc = N.mc)
#
# xx <- decision_tree_cluster(params = scenario_parameters[[1]][1:3, ],
#                             cost_dectree = "osNode_cost_2009_pdistn.Rds",
#                             health_dectree = "osNode_health_2009_pdistn.Rds")
