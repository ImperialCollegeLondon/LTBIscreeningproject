context("decision tree cluster")


library(treeSimR)
library(data.tree)
library(purrr)

data("scenario_parameters")

test_that("basic structure", {

  res <- decision_tree_cluster(parameters = scenario_parameters[[1]],
                               N.mc = 2,
                               n.uk_tb = 10,
                               n.exit_tb = 10,
                               cost_dectree = "osNode_cost_2009.Rds",
                               health_dectree = "osNode_cost_2009.Rds")

  expect_named(res, c("mc_cost", "mc_health", "n_tb_screen_all", "n_tb_screen_uk", "p_LTBI_to_effectiveTx", "subset_pop", "osNode.cost", "osNode.health", "call", "N.mc"))

  expect_length(res, 10)
  expect_length(res$mc_cost, 2)
  expect_length(res$mc_health, 2)
  expect_length(res$p_LTBI_to_effectiveTx, 1)

  expect_equal(res$N.mc, 2)

  expect_is(res, "list")
  expect_is(res$call, "call")

})



# res <- lapply(scenario_parameters,
#               decision_tree_cluster,
#               N.mc = N.mc,
#               n.uk_tb = n.uk_tb,
#               n.exit_tb = n.exit_tb)
#
# xx <- decision_tree_cluster(parameters = scenario_parameters[[1]][1:3, ],
#                             n.uk_tb = 10,
#                             n.exit_tb = 10,
#                             cost_dectree = "osNode_cost_2009_pdistn.Rds",
#                             health_dectree = "osNode_health_2009_pdistn.Rds")
