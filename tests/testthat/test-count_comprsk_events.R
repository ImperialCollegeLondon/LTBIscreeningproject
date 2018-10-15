context("test-count_comprsk_events.R")

library(dplyr)
library(reshape2)
library(data.table)

event_times <- list(tb = c(1,4,Inf),
                    exit_uk = c(2,3,4),
                    death = c(1,2,5))

dat <-
  event_times %>%
  count_comprsk_events()


test_that("return consistency", {

  n_pop <- dat$atrisk_start[1]

  # pop size equal over time
  expect_equal(rowSums(dat[ , paste0("total_", names(event_times))]),
               n_pop - dat[ ,"atrisk_end"])

  # event per year
  expect_equal(rowSums(dat[ ,names(event_times)]),
               dat[ ,"atrisk_start"] - dat[ ,"atrisk_end"])

})


test_that("order events", {

  # descending
  expect_true(all(diff(dat[ ,"atrisk_end"]) <= 0))
  expect_true(all(diff(dat[ ,"atrisk_start"]) <= 0))

  # ascending
  expect_true(all(diff(dat[ ,"total_exit_uk"]) >= 0))
  expect_true(all(diff(dat[ ,"total_death"]) >= 0))
  expect_true(all(diff(dat[ ,"total_tb"]) >= 0))

})

test_that("misc", {

  expect_equal(count_comprsk_events(list(tb = 1,
                                         exit_uk = 1,
                                         death = 1)),
               count_comprsk_events(c(tb = 1,
                                      exit_uk = 1,
                                      death = 1)))

  # first in list is priority
  dat2 <-
    count_comprsk_events(list(tb = 1,
                              exit_uk = 1,
                              death = 1))

  expect_true(all(dat2$total_tb == 1))
  expect_true(all(dat2$total_exit_uk == 0))
  expect_true(all(dat2$total_death == 0))

})
