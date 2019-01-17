context("test-activetb_qaly_cost.R")


library(dplyr)
library(purrr)
library(assertthat)
library(LTBIscreeningproject)
library(treeSimR)


dectree_res <-
  list(
    list(mc_cost = c(1, 2),
                    mc_health = c(2, 1),
                    subset_pop = as.matrix(data.frame(p_LTBI_to_cured = c(0.1, 0.9))),
                    N.mc = 2),
    list(mc_cost = c(1, 2),
         mc_health = c(2, 1),
         subset_pop = as.matrix(data.frame(p_LTBI_to_cured = c(0.1, 0.9))),
         N.mc = 2))


interv <- list(N.mc = 2,
               ENDPOINT_QALY = "death",
               ENDPOINT_cost = "death")

cohort <-
  data.frame(cfr = 1,
             all_tb = TRUE,
             uk_tb = TRUE,
             tb_fatality = FALSE,
             QALY_statusquo = 1,
             QALY_diseasefree = 2,
             QALY_cured = 1,
             QALY_fatality = 0.5,
             uk_notif_discounts = 1,
             all_notif_discounts = 1,
             uk_secondary_inf_discounts = 1,
             all_secondary_inf_discounts = 1,
             num_2nd_inf = 0,
             num_contacts = 0,
             id_avoided_tb = 1)


res <- activetb_qaly_cost(dectree_res,
                          interv,
                          cohort)


names_output <- c("QALY.statusquo", "QALY.screened", "E_cost_screened", "cost.screened_person", "cost.statusquo_person",
                  "cost_incur", "cost.statusquo", "cost.screened", "E_QALY_screened", "QALY.screened_person", "QALY.statusquo_person",
                  "QALYgain", "cost_incur_person", "E_cost_incur", "E_cost_incur_person", "QALYgain_person", "E_QALYgain", "E_QALYgain_person")

expected_names_output <- c("E_cost_screened", "E_QALY_screened", "E_cost_incur",  "E_cost_incur_person", "E_QALYgain", "E_QALYgain_person")
sim_names_output <- setdiff(names_output, expected_names_output)


test_that("i/o format", {

  expect_length(res, length(names_output))
  expect_named(res, names_output)

  # number of scenarios
  expect_true(all(map_df(res, length) == 2))

  # single values
  expect_true(all(map_df(map(res[expected_names_output], 1), length) == 1))

  # value per mc sim
  expect_true(all(map_df(map(res[sim_names_output], 1), length) == 2))

})


test_that("boundary - zero screening cost", {

  # useless/perfect treatment
  dectree_res <-
    list(
      list(mc_cost = c(0, 0),
           mc_health = c(2, 1),
           subset_pop = as.matrix(data.frame(p_LTBI_to_cured = c(0, 1))),
           N.mc = 2))

  res <- activetb_qaly_cost(dectree_res,
                            interv,
                            cohort)

  TB_Tx_cost <- 5410

  expect_equal(unlist(res$cost.statusquo), c(TB_Tx_cost, TB_Tx_cost))
  expect_equal(res$cost.statusquo_person, res$cost.statusquo)

  expect_equal(unlist(res$cost.screened), c(TB_Tx_cost, 0))
  expect_equal(res$cost.screened_person, res$cost.screened)

  expect_equal(unlist(res$cost_incur), c(0, -TB_Tx_cost))
  expect_equal(res$cost_incur_person, res$cost_incur)

  expect_equal(unlist(res$E_cost_screened), TB_Tx_cost/2)

  expect_equal(unlist(res$E_cost_incur), TB_Tx_cost/2 - TB_Tx_cost)

})


##TODO:
test_that("boundary - zero screening QALY loss", {

  # useless/perfect treatment
  # dectree_res <-
  #   list(
  #     list(mc_cost = c(0, 0),
  #          mc_health = c(2, 1),
  #          subset_pop = as.matrix(data.frame(p_LTBI_to_cured = c(0, 1))),
  #          N.mc = 2))
  #
  # res <- activetb_qaly_cost(dectree_res,
  #                           interv,
  #                           cohort)
  #
  # TB_Tx_cost <- 5410
  #
  # expect_equal(unlist(res$cost.statusquo), c(TB_Tx_cost, TB_Tx_cost))
  # expect_equal(res$cost.statusquo_person, res$cost.statusquo)
  #
  # expect_equal(unlist(res$cost.screened), c(TB_Tx_cost, 0))
  # expect_equal(res$cost.screened_person, res$cost.screened)
  #
  # expect_equal(unlist(res$cost_incur), c(0, -TB_Tx_cost))
  # expect_equal(res$cost_incur_person, res$cost_incur)
  #
  # expect_equal(unlist(res$E_cost_screened), TB_Tx_cost/2)
  #
  # expect_equal(unlist(res$E_cost_incur), TB_Tx_cost/2 - TB_Tx_cost)
  #
})
