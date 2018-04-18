context("test-count_comprsk_events.R")

library(dplyr)


test_that("return consistency", {

  indiv_event_times <- list(tb = c(1,4),
                            exit_uk = c(2,3),
                            death = c(1,2))

  dat <-
    indiv_event_times %>%
    count_comprsk_events() %>%
    t() %>%
    data.frame()

  expect_equal(max(rowSums(dat[ ,-1])), 2)

  expect_equal(max(rowSums(dat[ ,-1])),
               min(rowSums(dat[ ,-1])))

  })
