#
# project: LTBI screening
# N Green
# Oct 2016
#
# output cost-effectiveness plots


library(ggplot2)
library(BCEA)


# convert active TB lists to dataframes
aTB_cost_diff.df <- data.frame(Reduce(rbind, aTB_cost_diff))
aTB_QALYgain.df <- data.frame(Reduce(rbind, aTB_QALYgain))

scenario.names <- as.character(c(0, seq_len(n.scenarios)))

aTB_cost_diff.df <- t(rbind(0, aTB_cost_diff.df))
colnames(aTB_cost_diff.df) <- scenario.names

aTB_QALYgain.df <- t(rbind(0, aTB_QALYgain.df))
colnames(aTB_QALYgain.df) <- scenario.names


# convert LTBI screening dataframes
mc_cost_scenarios <- read.csv(file = "ext-data/mc_cost.csv", header = FALSE)
mc_health_scenarios <- read.csv(file = "ext-data/mc_health.csv", header = FALSE)

c.total <- t(rbind(0, mc_cost_scenarios))
colnames(c.total) <- scenario.names

e.total <- t(rbind(0, -mc_health_scenarios))
colnames(e.total) <- scenario.names


e.total <- e.total + aTB_QALYgain.df
c.total <- c.total + aTB_cost_diff.df



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




##TODO##

# money saved/cases averted over time

#status-quo

notif_eventtime <- na.omit(IMPUTED_sample_year_cohort$rNotificationDate_issdt)

CDF <- ecdf(notif_eventtime)
plot(CDF, yaxt='n', xlab="Days", main="Raw cumulative counts of active TB cases", pch = NA)
# axis(side = 2, at = 1, tck = 0.01, labels = sum(IMPUTED_sample$uk_tb), las = "2")
axis(side = 2, at = 1, tck = 0.01, labels = round(n.tb_year * aTB_TxDx_cost), las = "2") # direct cost of active TB diagnosis and treatment



# probability cost-effective for adherence vs uptake, for given costs per test
## graphs
## mesh/contour


