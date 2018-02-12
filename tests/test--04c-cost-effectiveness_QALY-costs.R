
IMPUTED_sample_year_cohort <-
  data.frame(
    uk_notif_discounts = c(1, NA),
    all_notif_discounts = c(1, 1),
    uk_secondary_inf_discounts = c(1, NA),
    all_secondary_inf_discounts = c(1, 1),
    QALY_statusquo = c(0.1, 0.5),
    QALY_diseasefree = c(1, 1),
    QALY_fatality = c(0.1, 0.1),
    QALY_cured = c(0.5, 0.5))


n.diseasefree.all_tb <- list(data.frame(sim = 1,
                                        status = "disease-free",
                                        n = 2))
n.diseasefree.uk_tb  <- list(data.frame(sim = 1,
                                        status = "disease-free",
                                        n = 1))
n.scenarios <- 1
n_all_tb <- num_all_tb_QALY <- 2
n_uk_tb  <- 1

s <- 1
N.mc <- 1
i <- 1

unit_cost.aTB_TxDx <- 10
num_sec_inf <- 2

ENDPOINT_cost <- "exit uk"

pop_year <- 10
