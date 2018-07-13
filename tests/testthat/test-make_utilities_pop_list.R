context("test-make_utilities_pop_list.R")

test_that("basics", {


  utility <- list(disease_free = 1,
                  activeTB = 1,
                  TB_Tx = 1,
                  postTx = 1)



  expect_is(make_utilities_pop_list(utility, n_pop = 1), 'list')
  expect_length(make_utilities_pop_list(utility, n_pop = 1), 3)

  expect_length(make_utilities_pop_list(utility, n_pop = 1)$diseasefree %>% unlist, 1)
  expect_length(make_utilities_pop_list(utility, n_pop = 1)$fatality %>% unlist, 2)
  expect_length(make_utilities_pop_list(utility, n_pop = 1)$cured %>% unlist, 3)

  expect_length(make_utilities_pop_list(utility, n_pop = 2)$cured, 2)


  utility$postTx <- 0
  expect_equivalent(make_utilities_pop_list(utility, n_pop = 1)$cured[[1]][3], 0)

})
