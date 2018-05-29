# *****************************************************
# LTBI screening
# N Green
# 2018
#
# gridded plots of the subpopulation sizes over time
# can do this on the raw, fitted, extrapolated or subsample data

# tb is for cases in EWNI only

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

# append screening subpop
screen <- table(ceiling(cohort$screen_year))
screen <- c(screen, rep(0, 100 - length(screen)))

strat_pop_year <- cbind(t(strat_pop_year), screen)


# append average costs and discounted costs
# for each scenario

strat_pop_year <-
  data.frame(strat_pop_year, row.names = NULL) %>%
  mutate(tb_diff = c(0, diff(tb)),
         discount = discount(t_limit = nrow(strat_pop_year)),
         c_tb = mean_cost.aTB_TxDx + (mean_num_sec_inf * mean_cost.aTB_TxDx)/1.035)

for (i in seq_len(n.scenarios)) {

  temp <-
    strat_pop_year %>%
    transmute(c_screen = mean(dectree_res[[i]]$mc_cost),
              t_screen = screen * c_screen,
              t_tb = tb_diff * c_tb,
              dis_tb = discount * t_tb,
              dis_screen = discount * t_screen)

  names(temp) <- paste0(names(temp), i)

  strat_pop_year <-
    cbind.data.frame(strat_pop_year, temp)
}


write.csv(strat_pop_year,
          file = pastef(diroutput, 'num-competing-events-by-year.csv'))

