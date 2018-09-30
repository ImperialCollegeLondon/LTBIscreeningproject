context("test-sim_tb_times.R")

test_that("dummy data", {

  data <-
    data.frame(date_death1_issdt.years = c(10,5,10),
               date_exit_uk1_issdt.years = c(5, Inf, 9),
               LTBI = c(TRUE, TRUE, TRUE),
               exit_uk1 = c(TRUE, FALSE, TRUE))

  prob <- c(0,0,1,0,0,0,0,1,0,0,0)

  expect_equal(sim_tb_times(data, prob),
               c(8, 3, Inf))

})
