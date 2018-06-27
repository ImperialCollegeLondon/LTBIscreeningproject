context("test-subset_pop_dectree.R")

library(purrr)
library(data.tree)


dectree <- treeSimR::costeffectiveness_tree(yaml_tree = "../../data/LTBI_dectree-cost.yaml")
osNode <- dectree$osNode

parameter_p <-
  tibble(
    node = "Effective",
    min = NA,
    max = NA,
    distn = NA,
    scenario = 1,
    val_type = "QALYloss",
    p = 0
  )

parameter_cost <-
  tibble(
    node = "Agree to Screen",
    min = 100,
    max = 100,
    distn = "unif",
    scenario = 1,
    val_type = "cost",
    p = NA
  )

treeSimR::assign_branch_values(osNode,
                               osNode,
                               parameter_p = parameter_p,
                               parameter_cost = parameter_cost)

osNode$Set(path_probs = treeSimR::calc_pathway_probs(osNode))

subset_pop <- subset_pop_dectree(osNode)
subset_pop2 <- subset_pop_dectree2(osNode)


test_that("order of subgroup sizes", {

  # all screen cohort

  expect_lt(as.numeric(subset_pop['positive']),
            as.numeric(subset_pop['tests']))

  expect_lt(as.numeric(subset_pop['startTx']),
            as.numeric(subset_pop['positive']))

  expect_lt(as.numeric(subset_pop['completeTx']),
            as.numeric(subset_pop['startTx']))

  expect_lt(as.numeric(subset_pop['cured']),
            as.numeric(subset_pop['completeTx']))

  # LTBI screen cohort

  expect_lt(as.numeric(subset_pop['LTBI_positive']),
            as.numeric(subset_pop['LTBI_tests']))

  expect_lt(as.numeric(subset_pop['LTBI_startTx']),
            as.numeric(subset_pop['LTBI_positive']))

  expect_lt(as.numeric(subset_pop['LTBI_completeTx']),
            as.numeric(subset_pop['LTBI_startTx']))

  # all vs LTBI

  expect_gt(as.numeric(subset_pop['LTBI_positive']),
            as.numeric(subset_pop['positive']))

  expect_gt(as.numeric(subset_pop['LTBI_startTx']),
            as.numeric(subset_pop['startTx']))

  expect_gt(as.numeric(subset_pop['LTBI_completeTx']),
            as.numeric(subset_pop['completeTx']))
})


test_that("prob constraints", {

  expect_lte(as.numeric(subset_pop)[1], 1)
  expect_gte(as.numeric(subset_pop)[1], 0)

  expect_lte(as.numeric(subset_pop)[2], 1)
  expect_gte(as.numeric(subset_pop)[2], 0)

  expect_lte(as.numeric(subset_pop)[3], 1)
  expect_gte(as.numeric(subset_pop)[3], 0)

  expect_lte(as.numeric(subset_pop)[4], 1)
  expect_gte(as.numeric(subset_pop)[4], 0)

  expect_lte(as.numeric(subset_pop)[5], 1)
  expect_gte(as.numeric(subset_pop)[5], 0)

  expect_lte(as.numeric(subset_pop)[6], 1)
  expect_gte(as.numeric(subset_pop)[6], 0)

  expect_equal(subset_pop$LTBI_pre,
               subset_pop$LTBI_post)
})
