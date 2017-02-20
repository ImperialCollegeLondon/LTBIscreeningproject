
# gridded plots of the subpopulation sizes over time
# can do this on the raw, fitted, extrapolated, subsample data

par(mfrow=c(2,2))

strat_plot <- pryr::partial(plot,
                            type = "s", xlim=c(0,20), xlab = "Year")

strat_plot(unlist(strat_pop_year["tb", ]),
     ylim = c(0,500), ylab="active TB")

strat_plot(unlist(strat_pop_year["death", ]),
     ylab = "all-cause death")

strat_plot(unlist(strat_pop_year["exit_uk", ]),
     ylim = c(0, max(unlist(strat_pop_year["exit_uk", ]))), ylab = "exit EWNI")

strat_plot(unlist(strat_pop_year["at-risk", ]),
      ylab = "remain in EWNI")

knitr::kable(t(strat_pop_year))
