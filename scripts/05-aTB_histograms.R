
windows(width = 100, height = 50)
par(mfrow = c(6,6))

for (i in seq_len(n.scenarios)) {

  hist(aTB_CE_stats$cost_incur_person[[i]], breaks = 15,
       main = i, xlim = c(-80,0))
  # abline(v = aTB_CE_stats$E_cost_incur_person[i], col = "red")
}

windows(width = 100, height = 50)
par(mfrow = c(6,6))

for (i in seq_len(n.scenarios)) {

  hist(aTB_CE_stats$QALYgain_person[[i]], breaks = 15,
       main = i, xlim = c(0,0.01))
  # abline(v = aTB_CE_stats$E_QALYgain_person[i], col = "red")
}
