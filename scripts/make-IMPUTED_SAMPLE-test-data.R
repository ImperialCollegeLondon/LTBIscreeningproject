
IMPUTED_sample_year_cohort <-
  data.frame(
    rNotificationDate_issdt.years = c(10, NA),
    all_tb_issdt = c(10, 20),
    tb_fatality = c(TRUE, TRUE)
    # all_death_rNotificationDate = c(30, 40),
    # age_all_notification = c(20, 30)
  )


n.diseasefree.all_tb <- list(data.frame(sim = 1,
                                        status = "disease-free",
                                        n = 2))
n.diseasefree.uk_tb  <- list(data.frame(sim = 1,
                                        status = "disease-free",
                                        n = 1))
n.scenarios <- 1
n_all_tb <- num_all_tb_QALY <- 2
n_uk_tb  <- 1


# QALY_all_tb <-
#   IMPUTED_sample_year_cohort %$%
#   calc_QALY_tb(timetoevent = all_death_rNotificationDate,
#                utility.disease_free = 1.0,
#                utility.case = 0.5,
#                age = age_all_notification)

QALY_all_tb <- list(diseasefree = c(1, 1),
                    fatality = c(0.1, 0.1),
                    cured = c(0.5, 0.5))

s <- 1
N.mc <- 1
i <- 1

unit_cost.aTB_TxDx <- 10
num_sec_inf <- 2

ENDPOINT_cost <- "exit uk"

pop_year <- 10
