##TODO: update money saved/cases averted over time

# money saved/cases averted over time -------------------------------------

# status-quo

notif_eventtime <- na.omit(IMPUTED_sample_year_cohort$rNotificationDate_issdt)

CDF <- ecdf(notif_eventtime)

plot(CDF, yaxt = 'n',
     xlab = "Days since entry",
     main = "Raw cumulative counts of active TB cases\n in EWNI", pch = NA)

# axis(side = 2, at = 1, tck = 0.01, labels = sum(IMPUTED_sample$uk_tb), las = "2")
axis(side = 2, at = 1, tck = 0.01,
     labels = round(n.uk_tb * unit_cost$aTB_TxDx), las = "2") # direct cost of active TB diagnosis and treatment




# probability cost-effective for adherence vs uptake, for given co --------
##TODO: update contour plot adherence vs uptake

COST <- 50 #20, 50, 100

# expected incremental net benefit (INB)
e.INMB <- plyr::ldply(.data = INMB,
                      .fun = mean)

dat <- gdata::cbindX(scenario_parameter_cost,
                     scenario_parameter_p,
                     e.INMB)

names(dat) <-
  names(dat) %>%
  make.names(unique = TRUE)

dat.plot <- dat[dat$Agree.to.Screen == COST, ] #specific unit cost
# dat.plot <- dat.plot[dat.plot$Agree.to.Screen.1==0.1, ] #specific screening uptake


# contour
ggplot(data = dat.plot, aes(x = Start.Treatment, y = Complete.Treatment, z = V1)) +
  theme_bw() +
  stat_contour(show.legend = TRUE)

# coloured mesh
ggplot(dat.plot, aes(x = Start.Treatment, y = Complete.Treatment, z = V1)) +
  stat_contour(geom = 'polygon', aes(fill = ..level..)) +
  geom_tile(aes(fill = V1)) +
  #stat_contour(bins = 15) +
  xlab('Start treatment') +
  ylab('Complete treatment') +
  guides(fill = guide_colorbar(title = ''))


e.INMB.sq <- matrix(dat.plot$V1, 21)

filled.contour(x = seq(0, 1, by = 0.05),
               y = seq(0, 1, by = 0.05), e.INMB.sq,
               color = terrain.colors,
               xlab = "Completed Treatment", ylab = "Started Treatment",
               #plot.axes = { axis(1, seq(100, 800, by = 100))
               #  axis(2, seq(100, 600, by = 100)) },
               key.title = title(main = "INMB\n(GBP)"),
               nlevels = 5)
#key.axes = axis(4, seq(90, 190, by = 10)))  # maybe also asp = 1
