context("test-set_branch_uniform_params.R")

vals <-
  list(pmin = 0.33,
       pmax = 0.84,
       name = "bad name")

class(vals) <- c("branch_unif_params", "list")

test_that("error", {

  expect_error(set_branch_uniform_params(vals, osNode))
})
