
library(QALY)
context("Calculate QALYs")


test_that("Compare different scenario QALYs", {

  QALY_uk_tb <- calc_QALY_uk_tb(data = IMPUTED_sample_year_cohort,
                                utility$disease_free,
                                utility.case = utility$activeTB,
                                endpoint = "death")

  testthat::expect_true(all(with(QALY_uk_tb, diseasefree - cured)>=0))
  testthat::expect_true(all(with(QALY_uk_tb, diseasefree - fatality)>=0))
  testthat::expect_true(all(with(QALY_uk_tb, cured - fatality)>=0))
})



