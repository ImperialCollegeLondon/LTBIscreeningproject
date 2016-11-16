#
# project: LTBI screening
# N Green
# Oct 2016
#
# output cost-effectiveness plots


library(ggplot2)
library(BCEA)


# convert to dataframes
aTB_cost_diff.df <- data.frame(Reduce(rbind, aTB_cost_diff))
aTB_QALYgain.df <- data.frame(Reduce(rbind, aTB_QALYgain))

aTB_cost_diff.df <- t(rbind(0, aTB_cost_diff.df))
colnames(aTB_cost_diff.df) <- as.character(0:nrow(mc_cost_scenarios))

aTB_QALYgain.df <- t(rbind(0, aTB_QALYgain.df))
colnames(aTB_QALYgain.df) <- as.character(0:nrow(mc_cost_scenarios))


# multiple scenarios
mc_cost_scenarios <- read.csv(file = "ext-data/mc_cost.csv", header = FALSE)
mc_health_scenarios <- read.csv(file = "ext-data/mc_health.csv", header = FALSE)

c.total <- t(rbind(0, mc_cost_scenarios))
colnames(c.total) <- as.character(0:nrow(mc_cost_scenarios))

e.total <- t(rbind(0, -mc_health_scenarios))
colnames(e.total) <- as.character(0:nrow(mc_health_scenarios))

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

# probability cost-effective for adherence vs uptake, for given costs per test
## graphs
## mesh/contour


