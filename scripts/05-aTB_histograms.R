#
# LTBI screening
# N Green
#
# compare active TB QALY gains and cost plots
#


load("C:/Users/ngreen1/Dropbox/TB/LTBI/R/LTBIscreeningproject/ext-data/18_to_35_in_2009/programme_level_scenario_038/aTB_CE_stats.RData")
load("C:/Users/ngreen1/Dropbox/TB/LTBI/R/LTBIscreeningproject/ext-data/18_to_35_in_2009/programme_level_scenario_044/aTB_CE_stats.RData")

n.scenarios <- length(aTB_CE_stats[[1]])

windows(width = 100, height = 50)
par(mfrow = c(6,6))

for (i in seq_len(n.scenarios)) {

  hist(aTB_CE_stats$cost_incur_person[[i]], breaks = 10,
       main = i)#, xlim = c(-80,0))
  # abline(v = aTB_CE_stats$E_cost_incur_person[i], col = "red")
}

windows(width = 100, height = 50)
par(mfrow = c(6,6))

for (i in seq_len(n.scenarios)) {

  hist(aTB_CE_stats$QALYgain_person[[i]], breaks = 10,
       main = i)#, xlim = c(0,0.01))
  # abline(v = aTB_CE_stats$E_QALYgain_person[i], col = "red")
}
