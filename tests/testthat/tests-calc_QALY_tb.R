context("Calculate QALYs")

library(QALY)
library(treeSimR)
library(dplyr)
library(memoise)


unit_intervals <- data.frame(symptoms_to_Tx = c(1,1,1),
                             Tx_to_cured = c(1,1,1),
                             cured_to_death = c(1,1,1))

age_all_notification <- rep(20,3)
start_delay <- rep(0,3)


test_that("Basics", {

  QALY_tb <- calc_QALY_tb(intervals = unit_intervals,
                          utility = list(disease_free = 1,
                                         postTx = 1,
                                         activeTB = 0.9,
                                         TB_Tx = 0.8),
                          age = age_all_notification,
                          start_delay = start_delay)

  # size order
  expect_true(all(with(QALY_tb, diseasefree - cured) >= 0))
  expect_true(all(with(QALY_tb, diseasefree - fatality) >= 0))
  expect_true(all(with(QALY_tb, cured - fatality) >= 0))

  expect_length(QALY_tb, 3)
  expect_length(QALY_tb$diseasefree, 3)
  expect_length(QALY_tb$fatality, 3)
  expect_length(QALY_tb$cured, 3)

  expect_equal(QALY_tb$diseasefree[1], QALY_tb$diseasefree[2])
  expect_equal(QALY_tb$fatality[1], QALY_tb$fatality[2])
  expect_equal(QALY_tb$cured[1], QALY_tb$cured[2])


  # compare ages
  QALY_tb_50y <- calc_QALY_tb(intervals = unit_intervals,
                              utility = list(disease_free = 1,
                                             postTx = 1,
                                             activeTB = 0.9,
                                             TB_Tx = 0.8),
                              age = c(50,50,50),
                              start_delay = start_delay)

  expect_true(all(QALY_tb$diseasefree > QALY_tb_50y$diseasefree))
  expect_true(all(QALY_tb$cured > QALY_tb_50y$cured))
  expect_true(all(QALY_tb$fatality > QALY_tb_50y$fatality))

})


test_that("edge cases", {

  QALY <-
    calc_QALY_tb(
      intervals = unit_intervals,
      utility = list(disease_free = 1,
                     postTx = 1,
                     activeTB = 1,
                     TB_Tx = 1),
      age = NA,
      start_delay = c(0,0,0),
      discount_rate = 0
    )

  expect_true(all(QALY$fatality == 1))
  expect_equivalent(QALY$cured[1], 3)
  expect_equivalent(QALY$diseasefree[1], 3)

  expect_equal(QALY$cured, QALY$diseasefree)

})


test_that("specific values", {


})


