context("test-activetb_qaly_cost.R")



dectree_res <-
  list(
    list(mc_cost = c(1, 2),
                    mc_health = c(2, 1),
                    subset_pop = as.matrix(data.frame(p_LTBI_to_cured = c(0.1, 0.9))),
                    N.mc = 2),
    list(mc_cost = c(1, 2),
         mc_health = c(2, 1),
         subset_pop = as.matrix(data.frame(p_LTBI_to_cured = c(0.1, 0.9))),
         N.mc = 2))


interv <- list(N.mc = 2,
               ENDPOINT_QALY = "death",
               ENDPOINT_cost = "death")

cohort <-
  data.frame(cfr = 1,
             all_tb = TRUE,
             uk_tb = TRUE,
             tb_fatality = FALSE,
             QALY_statusquo = 1,
             QALY_diseasefree = 2,
             QALY_cured = 1,
             QALY_fatality = 0.5,
             uk_notif_discounts = 1,
             all_notif_discounts = 1,
             uk_secondary_inf_discounts = 1,
             all_secondary_inf_discounts = 1,
             num_2nd_inf = 0,
             num_contacts = 0,
             id_avoided_tb = 1)


res <- activetb_qaly_cost(dectree_res,
                          interv,
                          cohort)


test_that("i/o format", {
  expect_equal()
})

test_that("boundary", {
  expect_equal()
})
