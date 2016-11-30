#
# project: LTBI screening
# N Green
# Oct 2016
#
# output cost-effectiveness plots


library(ggplot2)
library(BCEA)


if(!exists("aTB_cost_diff")) load(paste(diroutput, "aTB_cost_diff.RData", sep="/"))
if(!exists("aTB_QALYgain"))  load(paste(diroutput, "aTB_QALYgain.RData", sep="/"))


# convert active TB lists to dataframes
aTB_cost_diff.df <- data.frame(Reduce(rbind, aTB_cost_diff))
aTB_QALYgain.df <- data.frame(Reduce(rbind, aTB_QALYgain))

scenario.names <- as.character(c(0, seq_len(n.scenarios)))

aTB_cost_diff.df <- t(rbind(0, aTB_cost_diff.df))
colnames(aTB_cost_diff.df) <- scenario.names

aTB_QALYgain.df <- t(rbind(0, aTB_QALYgain.df))
colnames(aTB_QALYgain.df) <- scenario.names


# convert LTBI screening dataframes
mc_cost_scenarios <- read.csv(file = paste(diroutput, "mc_cost.csv", sep="/"), header = FALSE)
mc_health_scenarios <- read.csv(file = paste(diroutput, "mc_health.csv", sep="/"), header = FALSE)

c.total <- t(rbind(0, mc_cost_scenarios))
colnames(c.total) <- scenario.names

e.total <- t(rbind(0, -mc_health_scenarios))
colnames(e.total) <- scenario.names

popscale <- 100000

e.total <- (e.total + aTB_QALYgain.df) * popscale
c.total <- (c.total + aTB_cost_diff.df) * popscale



#########
# plots #
#########

screen.bcea <- bcea(e = e.total,
                    c = c.total,
                    ref = 1,
                    interventions = colnames(e.total))

ceplane.plot(screen.bcea, pos = "topleft")
ceplane.plot(screen.bcea, graph = "ggplot2")
contour(screen.bcea)
contour2(screen.bcea, graph="ggplot2")
eib.plot(screen.bcea)
ceac.plot(screen.bcea)




# money saved/cases averted over time

#status-quo

notif_eventtime <- na.omit(IMPUTED_sample_year_cohort$rNotificationDate_issdt)

CDF <- ecdf(notif_eventtime)
plot(CDF, yaxt='n', xlab="Days", main="Raw cumulative counts of active TB cases", pch = NA)
# axis(side = 2, at = 1, tck = 0.01, labels = sum(IMPUTED_sample$uk_tb), las = "2")
axis(side = 2, at = 1, tck = 0.01, labels = round(n.tb_year * aTB_TxDx_cost), las = "2") # direct cost of active TB diagnosis and treatment



# probability cost-effective for adherence vs uptake, for given costs per test

COST <- 50 #20, 50, 100

# expected incremental net benefit
e.INMB <- plyr::ldply(INMB, mean)

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




