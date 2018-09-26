context("test-assign_ltbi_status")

load(here::here("data", "cost_effectiveness_params.RData"))


test_that("aggregation matches input", {

  data_agg <- prop.table(table(IMPUTED_sample$LTBI,
                   IMPUTED_sample$who_inc_Pareek2011), margin = 2)
  data_agg['1', 1] <- 0.03

  expect_equal(pLatentTB.who, data_agg['1',], tolerance = 0.01)
})
