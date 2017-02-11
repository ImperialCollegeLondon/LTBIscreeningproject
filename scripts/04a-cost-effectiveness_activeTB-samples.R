#
# project: LTBI screening
# N Green
# Oct 2016
#
# sample how many LTBI become active cases in the UK
# with potential screening
# for each scenario


library(survival)
library(mstate)
library(cmprsk) # http://www.stat.unipg.it/luca/R
library(reshape)
library(data.table)


# create variables --------------------------------------------------------

# read-in random samples of proportion of LTBI -> disease-free
if(!exists("p.complete_treat_scenarios")){

  p.complete_treat_scenarios <- read.csv(file = paste(diroutput, "prob_complete_Tx_given_LTBI_by_who.csv", sep = "/"),
                                         header = FALSE) %>%
                                         set_names(who_levels)
  p.complete_treat_scenarios$scenario <- rownames(p.complete_treat_scenarios)
}

# LTBI probability look-up table
pLTBI_hash <- p.complete_treat_scenarios %>%
                gather("who_prev_cat_Pareek2011", "value", -scenario)

# labels of active TB case samples
uk_tbX_names <- paste0("uk_tb", seq_len(N.mc))

n.tb_screen <- list()

uk_tb_TRUE_year <- IMPUTED_sample_year_cohort$uk_tb==1




# sample active TB cases --------------------------------------------------

#####################################
# list of tables of samples         #
# of cases for each scenario        #
# and sample LTBI to disease-free   #
#####################################


for (scenario_i in seq_len(n.scenarios)){

  print(sprintf("scenario: %d", scenario_i))

  # individual active TB status for each scenario
  uk_tb_scenarios <- c(NULL, NULL)

  p.completeTx <- pLTBI_hash[pLTBI_hash$scenario==scenario_i, ] %>%
                    as.data.table()

  setkey(p.completeTx, "who_prev_cat_Pareek2011")

  # prob successfully completing LTBI Tx for each cohort individual
  who_prev_cat <- as.character(IMPUTED_sample_year_cohort$who_prev_cat_Pareek2011)
  p.complete_treat_sample <- p.completeTx[who_prev_cat, value]

  # sample new active TB status for N.mc samples
  for (tbsample in uk_tbX_names){

    uk_tbX_after_screen <- sample_uk_tb(prob = p.complete_treat_sample, is.tb = uk_tb_TRUE_year)

    uk_tb_scenarios <- rbind(uk_tb_scenarios,
                             data.frame(sim = tbsample,
                                        status = uk_tbX_after_screen))
  }

  # counts of non/active TB cases _after_ screening
  n.tb_screen[[scenario_i]] <- uk_tb_scenarios %>%
                               group_by(sim, status) %>%
                               tally()
  # rename tb status
  n.tb_screen[[scenario_i]]$status <- plyr::revalue(as.factor(n.tb_screen[[scenario_i]]$status),
                                                  c("0" = "disease-free", "1" = "uk_tb"))


  # save --------------------------------------------------------------------

  filename <- sprintf("uk_tb_scenarios - scenario_%d.RData", scenario_i)
  save(uk_tb_scenarios, file = paste(diroutput, filename, sep = "/"))

  filename <- sprintf("n.tb_screen - scenario_%d.RData", scenario_i)

  n.tb_screen_scenario <- n.tb_screen[[scenario_i]]
  save(n.tb_screen_scenario, file = paste(diroutput, filename, sep = "/"))
}
