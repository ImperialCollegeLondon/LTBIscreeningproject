context("test-rows_first_n_ids.R")

test_that("simple operations", {

  expect_length(rows_first_n_ids(id_avoid = 1:10,
                                   prop_avoid = 1), 10)

  expect_true(all(rows_first_n_ids(id_avoid = 1:10,
                                   prop_avoid = 1)))

  expect_false(all(rows_first_n_ids(id_avoid = 1:10,
                                    prop_avoid = 0)))

  expect_equal(sum(rows_first_n_ids(id_avoid = 1:10,
                                    prop_avoid = 0.5)), 5)

  expect_equal(sum(rows_first_n_ids(id_avoid = 10:1,
                                    prop_avoid = 0.5)), 5)

})
