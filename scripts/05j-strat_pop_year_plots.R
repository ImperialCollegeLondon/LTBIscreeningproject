# *****************************************************
# LTBI screening
# N Green
# 2018
#
# gridded plots of the subpopulation sizes over time
# can do this on the raw, fitted, extrapolated or subsample data


event_times <- list(tb = cohort$rNotificationDate_issdt.years,
                    exit_uk = cohort$date_exit_uk1_issdt.years,
                    death = cohort$date_death1_issdt.years)

strat_pop_year <- count_comprsk_events(event_times)


filename <- paste(plots_folder_scenario, "time-to-event-grid-plot.png", sep = "/")

png(filename, width = 400, height = 350, res = 45)

par(mfrow = c(2,2))

strat_plot <- pryr::partial(plot,
                            type = "s", xlim = c(0,20), xlab = "Year")

strat_plot(unlist(strat_pop_year["tb", ]),
     ylim = c(0,500), ylab = "active TB")

strat_plot(unlist(strat_pop_year["death", ]),
     ylab = "all-cause death")

strat_plot(unlist(strat_pop_year["exit_uk", ]),
     ylim = c(0, max(unlist(strat_pop_year["exit_uk", ]))), ylab = "exit EWNI")

strat_plot(unlist(strat_pop_year["at-risk", ]),
      ylab = "remain in EWNI")

dev.off()


# knitr::kable(t(strat_pop_year))

screen <- table(ceiling(cohort$screen_year))
screen <- c(screen, rep(0, 100 - length(screen)))
strat_pop_year <- cbind(t(strat_pop_year), screen)

write.csv(strat_pop_year, file =  pastef(diroutput, 'num-competing-events-by-year.csv'))

