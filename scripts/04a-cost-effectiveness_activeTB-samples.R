#
# project: LTBI screening
# N Green
# Oct 2016
#
# sample how many LTBI become active cases
# with screening for each scenario


##TODO##
# extrapolate TB activation past ETS follow-up


library(survival)
library(mstate)
library(cmprsk) # http://www.stat.unipg.it/luca/R
library(reshape)
library(data.table)


# create variables --------------------------------------------------------

# read-in random samples of proportion of LTBI -> disease-free
if(!exists("p.complete_treat_scenarios")){

  p.complete_treat_scenarios <- read.csv(file = paste(diroutput, "prob_complete_Tx_given_LTBI_by_who.csv", sep = "/"), header = FALSE)
  names(p.complete_treat_scenarios) <- who_levels
  p.complete_treat_scenarios$scenario <- rownames(p.complete_treat_scenarios)
}

# look-up table
hash <-
  p.complete_treat_scenarios %>%
  gather("who_prev_cat_Pareek2011", "value", -scenario)

# number of active TB case samples
uk_tbX_names <- paste("uk_tb", seq_len(N.mc), sep = "")

n.tb_screen <- list()

uk_tb_TRUE_year <- IMPUTED_sample_year_cohort$uk_tb==1



#################################
# tables of number of active TB #
# cases for each scenario and   #
# sample LTBI to disease-free   #
#################################


for (scenario in seq_len(n.scenarios)){

  print(sprintf("scenario: %d", scenario))

  # individual tb status for each scenario
  uk_tb_scenarios <- c(NULL, NULL)

  p.completeTx <- data.table(hash[hash$scenario==scenario, ])
  setkey(p.completeTx, "who_prev_cat_Pareek2011")

  # prob completing LTBI Tx for each cohort individual
  who_prev_cat <- as.character(IMPUTED_sample_year_cohort$who_prev_cat_Pareek2011)
  p.complete_treat_sample <- p.completeTx[who_prev_cat, value]

  # sample new tb status for N.mc samples
  for (tbsample in uk_tbX_names){

    uk_tbX_after_screen <- sample_uk_tb(prob = p.complete_treat_sample, is.tb = uk_tb_TRUE_year)

    uk_tb_scenarios <- rbind(uk_tb_scenarios,
                             data.frame(sim = tbsample,  status = uk_tbX_after_screen))
  }


  # counts of active TB cases _after_ screening
  n.tb_screen[[scenario]] <- uk_tb_scenarios %>%
                              group_by(sim, status) %>%
                              tally()
  # rename tb status
  n.tb_screen[[scenario]]$status <- plyr::revalue(as.factor(n.tb_screen[[scenario]]$status),
                                                  c("0"="disease-free", "1"="uk_tb"))

  filename <- sprintf("uk_tb_scenarios - scenario_%d.RData", scenario)
  save(uk_tb_scenarios, file = paste(diroutput, filename, sep="/"))

  filename <- sprintf("n.tb_screen - scenario_%d.RData", scenario)

  n.tb_screen_scenario <- n.tb_screen[[scenario]]
  save(n.tb_screen_scenario, file = paste(diroutput, filename, sep="/"))
}
