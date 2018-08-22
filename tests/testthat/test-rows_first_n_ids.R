context("test-rows_first_n_ids.R")

test_that("simple operations", {

  rows_first_n_ids(id_avoid = 1:10,
                   prop_avoid = 1)

  rows_first_n_ids(id_avoid = 1:10,
                   prop_avoid = 0)

  rows_first_n_ids(id_avoid = 1:10,
                   prop_avoid = 0.5)

  rows_first_n_ids(id_avoid = 10:1,
                   prop_avoid = 0.5)

})
