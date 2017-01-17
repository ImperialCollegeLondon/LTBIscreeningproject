#
# project: LTBI screening
# N Green
# Oct 2016
#
# output cost-effectiveness plots
# cost-effectiveness planes
# costs incurred and QALY gains


library(ggplot2)
library(BCEA)


if(!exists("aTB_CE_stats")) load(paste(diroutput, "aTB_CE_stats.RData", sep = "/"))


###############
## active TB ##
###############

# convert active TB lists to dataframes
aTB_cost_diff.melt <- data.frame(Reduce(rbind, aTB_CE_stats$aTB_cost_diff_person), row.names = NULL)
aTB_QALYgain.melt  <- data.frame(Reduce(rbind, aTB_CE_stats$aTB_QALYgain_person), row.names = NULL)

# with status-quo
scenario.names <- as.character(c(0, seq_len(n.scenarios)))

# BCEA format
# append status-quo scenario
aTB_cost_diff.df <- t(rbind(0, aTB_cost_diff.melt)) %>%
                      as.data.frame() %>%
                      set_names(scenario.names)

aTB_QALYgain.df  <- t(rbind(0, aTB_QALYgain.melt)) %>%
                      as.data.frame() %>%
                      set_names(scenario.names)


####################
## LTBI screening ##
####################

# convert LTBI screening dataframes
LTBI_cost_melt <- read.csv(file = paste(diroutput, "mc_cost.csv", sep = "/"), header = FALSE)
LTBI_QALYloss_melt <- read.csv(file = paste(diroutput, "mc_health.csv", sep = "/"), header = FALSE)

# BCEA format
# append status-quo scenario
LTBI_cost.df <- t(rbind(0, LTBI_cost_melt)) %>%
                  as.data.frame() %>%
                  set_names(scenario.names)

# NB negative QALY loss is required QALY gain
LTBI_QALYgain.df <- t(rbind(0, - LTBI_QALYloss_melt)) %>%
                    as.data.frame() %>%
                    set_names(scenario.names)


popscale <- 100000

c.total <- as.matrix(LTBI_cost.df + aTB_cost_diff.df) * popscale
e.total <- as.matrix(LTBI_QALYgain.df + aTB_QALYgain.df) * popscale


screen.bcea <- bcea(e = -e.total,
                    c =  c.total,
                    ref = 1,
                    interventions = colnames(e.total))

#########
# plots #
#########

##TODO: how to get more than 8 colours in ggplot??
# COL <- colorRampPalette(brewer.pal(11, "Spectral"))(10)


# cost-effectiveness planes -----------------------------------------------

ceplane.plot(screen.bcea, pos = "topleft")
ceplane.plot(screen.bcea, graph = "ggplot2")
contour(screen.bcea)
gg <- contour2(screen.bcea, graph = "ggplot2")
gg + scale_color_brewer(palette = "Dark2")  #in colour


eib.plot(screen.bcea)
ceac.plot(screen.bcea)





# money saved/cases averted over time -------------------------------------

# status-quo

notif_eventtime <- na.omit(IMPUTED_sample_year_cohort$rNotificationDate_issdt)

CDF <- ecdf(notif_eventtime)

plot(CDF, yaxt = 'n',
     xlab = "Days since entry",
     main = "Raw cumulative counts of active TB cases\n in EWNI", pch = NA)

# axis(side = 2, at = 1, tck = 0.01, labels = sum(IMPUTED_sample$uk_tb), las = "2")
axis(side = 2, at = 1, tck = 0.01,
     labels = round(n.tb_year * unit_cost$aTB_TxDx), las = "2") # direct cost of active TB diagnosis and treatment




# probability cost-effective for adherence vs uptake, for given co --------
##TODO: update contour plot adherence vs uptake

COST <- 50 #20, 50, 100

# expected incremental net benefit (INB)
e.INMB <- plyr::ldply(.data = INMB,
                      .fun = mean)

dat <- gdata::cbindX(scenario_parameter_cost,
                     scenario_parameter_p,
                     e.INMB)

names(dat) <- make.names(names(dat), unique = TRUE)

dat.plot <- dat[dat$Agree.to.Screen==COST, ] #specific unit cost
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

filled.contour(x = seq(0,1,by = 0.05), y = seq(0,1,by = 0.05), e.INMB.sq,
               color = terrain.colors, xlab="Completed Treatment", ylab="Started Treatment",
               #plot.axes = { axis(1, seq(100, 800, by = 100))
               #  axis(2, seq(100, 600, by = 100)) },
               key.title = title(main = "INMB\n(GBP)"),
               nlevels = 5)
               #key.axes = axis(4, seq(90, 190, by = 10)))  # maybe also asp = 1




