context("test-subset_pop_dectree.R")


dectree <- treeSimR::costeffectiveness_tree(yaml_tree = "../../data/LTBI_dectree-cost.yaml")
osNode <- dectree$osNode

test_that("against monte carlo samples", {

  parameter_p <- tibble(node = "Effective", min = NA, max = NA, distn = NA,
                        scenario = 1, val_type = "QALYloss", p = 0)

  parameter_cost <- tibble(node = "Agree to Screen", min = 100, max = 100, distn = "unif",
                        scenario = 1, val_type = "cost", p = NA)

  treeSimR::assign_branch_values(osNode,
                                 osNode,
                                 parameter_p = parameter_p,
                                 parameter_cost = parameter_cost)

  osNode.cost$Set(path_probs = treeSimR::calc_pathway_probs(osNode.cost))

  subset_pop <- subset_pop_dectree(osNode.cost)

  p_LTBI_to_cured <- with(subset_pop, cured/LTBI_pre)

  n_tb_screen <- treeSimR::MonteCarlo_n.tb_screen(p_LTBI_to_cured,
                                                  n.uk_tb = 10,
                                                  n.all_tb = 100,
                                                  n = 2)

  uk_tb_screen <- n_tb_screen$n.tb_screen.uk_tb
  all_tb_screen <- n_tb_screen$n.tb_screen.all_tb

  mean_uk_diseasefree <-
    subset(uk_tb_screen$n,
           uk_tb_screen$status == 'disease-free') %>%
    mean()

  mean_uk_tb <-
    subset(uk_tb_screen$n,
           uk_tb_screen$status == 'tb') %>%
    mean()

  mean_all_diseasefree <-
    subset(all_tb_screen$n,
           all_tb_screen$status == 'disease-free') %>%
    mean()

  mean_all_tb <-
    subset(all_tb_screen$n,
           all_tb_screen$status == 'tb') %>%
    mean()

  expect_equal(p_LTBI_to_cured*10, mean_uk_diseasefree, tolerance = 0.01)
  expect_equal(p_LTBI_to_cured*100, mean_all_diseasefree, tolerance = 0.01)

  expect_equal((1 - p_LTBI_to_cured)*10, mean_uk_tb, tolerance = 0.01)
  expect_equal((1 - p_LTBI_to_cured)*100, mean_all_tb, tolerance = 0.01)

  expect_equal(subset_pop$LTBI_pre, subset_pop$LTBI_post)
})
