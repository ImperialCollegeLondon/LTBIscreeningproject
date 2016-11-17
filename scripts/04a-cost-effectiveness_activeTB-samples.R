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

# read-in random sample of proportion of LTBI -> disease-free
p.complete_treat_scenarios <- read.csv(file = "ext-data/prob_complete_Tx_given_LTBI_by_who.csv", header = FALSE)
names(p.complete_treat_scenarios) <- who_levels
p.complete_treat_scenarios$scenario <- rownames(p.complete_treat_scenarios)

hash <- melt(p.complete_treat_scenarios,
             variable_name = "who_prev_cat_Pareek2011")


n.uk_tbX <- N.mc
uk_tbX_names <- paste("uk_tb", seq_len(n.uk_tbX), sep = "")

# individual tb status for each scenario
uk_tb_scenarios <- as.data.frame(matrix(IMPUTED_sample$uk_tb,
                                        nrow = n.pop,
                                        ncol = n.uk_tbX, byrow = FALSE))
names(uk_tb_scenarios) <- uk_tbX_names
n.tb_screen <- list()



#################################
# tables of number of active TB #
# cases for each scenario and   #
# sample LTBI to disease-free   #
#################################

for (scenario in seq_len(n.scenarios)){

  p.completeTx <- data.table(hash[hash$scenario==scenario, ])
  setkey(p.completeTx, "who_prev_cat_Pareek2011")

  # prob completing LTBI Tx for each cohort individual
  p.complete_treat_sample <- p.completeTx[as.character(IMPUTED_sample$who_prev_cat_Pareek2011), value]

  # sample new tb status for n.uk_tbX samples
  for (tbsample in uk_tbX_names){

    uk_tb_scenarios[uk_tb_TRUE, tbsample] <- uk_tb_after_screen(uk_tb_TRUE,
                                                                p.complete_treat_sample)
  }

  # number active TB cases _after_ screening
  n.tb_screen[[scenario]] <- apply(uk_tb_scenarios, 2, table)
  rownames(n.tb_screen[[scenario]]) <- c("disease-free", "uk_tb")

  ##TODO##
  #data.table version of table()
}
