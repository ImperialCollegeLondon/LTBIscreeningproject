context("test-count_comprsk_events.R")

library(dplyr)

event_times <- list(tb = c(1,4,Inf),
                    exit_uk = c(2,3,4),
                    death = c(1,2,5))

dat <-
  event_times %>%
  count_comprsk_events() %>%
  t() %>%
  data.frame()


test_that("return consistency", {


  expect_equal(max(rowSums(dat[ ,-1])), 2)

  expect_equal(max(rowSums(dat[ ,-1])),
               min(rowSums(dat[ ,-1])))

  })


test_that("order events", {


})
