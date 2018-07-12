context("Calculate QALYs")

library(QALY)
library(treeSimR)
library(dplyr)
library(memoise)


test_that("Compare between different outcome QALYs", {

  time_to_event <- c(10,2,3,4,5,6,3,2,12,3,4,56,6,7,4,3,2,34,5,6)
  age_all_notification <- rep(20,20)
  start_delay <- rep(0,20)

  QALY_tb <- calc_QALY_tb(timetoevent = time_to_event,
                          utility = list(disease_free = 1,
                                         postTx = 1,
                                         activeTB = 0.9),
                          age = age_all_notification,
                          start_delay = start_delay)

  expect_true(all(with(QALY_tb, diseasefree - cured) >= 0))
  expect_true(all(with(QALY_tb, diseasefree - fatality) >= 0))
  expect_true(all(with(QALY_tb, cured - fatality) >= 0))


  QALY_tb_50y <- calc_QALY_tb(timetoevent = time_to_event,
                              utility = list(disease_free = 1,
                                             postTx = 1,
                                             activeTB = 0.9),
                              age = rep(50,20),
                              start_delay = start_delay)

  expect_true(all(QALY_tb$diseasefree > QALY_tb_50y$diseasefree))
  expect_true(all(QALY_tb$cured > QALY_tb_50y$cured))
  expect_true(all(QALY_tb$fatality > QALY_tb_50y$fatality))

})

test_that("edge cases", {

  QALY <-
      calc_QALY_tb(
      timetoevent = all_death_notif,
      # timetoevent = 2,
      # utility = utility,
      utility = list(disease_free = 1, activeTB = 1, postTx = 1),
      # age = age_all_notification,
      age = NA,
      start_delay = 0,
      discount_rate = 0
    )



})



