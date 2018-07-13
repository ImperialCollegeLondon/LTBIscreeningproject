context("test-make_intervals_pop_list.R")

test_that("basics", {

  intervals <-
    data.frame(symptoms_to_Tx = c(1,1),
               Tx_to_cured = c(1,1),
               cured_to_death = c(1,1))

  res <- make_intervals_pop_list(intervals)

  expect_is(res, 'list')
  expect_length(res, 3)

  expect_length(res$diseasefree, 2)
  expect_length(res$cured, 2)
  expect_length(res$fatality, 2)

  expect_length(res$diseasefree[[1]], 1)
  expect_length(res$fatality$`1`, 2)
  expect_length(res$cured$`1`, 3)

})
