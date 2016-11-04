#
# project: LTBI screening
# N Green
# Oct 2016
#
# output plots screening decision tree


library(ggplot2)
library(BCEA)


# single intervention
e.total <- matrix(c(rep(0,N.mc), -mc.health$`expected values`), #) + aTB_QALYloss[1]),
                  ncol=2, dimnames = list(NULL, c("donothing","interv")))
c.total <- matrix(c(rep(0,N.mc), mc.cost$`expected values`), # + aTB_cost[1])
                  ncol=2, dimnames = list(NULL, c("donothing","interv")))


# multiple scenarios intervention
mc_cost_scenarios <- read.csv(file="mc_cost.csv", header = FALSE)
mc_health_scenarios <- read.csv(file="mc_health.csv", header = FALSE)

c.total <- t(rbind(0, mc_cost_scenarios))
colnames(c.total) <- as.character(0:nrow(mc_cost_scenarios))
e.total <- t(rbind(0, -mc_health_scenarios))
colnames(e.total) <- as.character(0:nrow(mc_health_scenarios))


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
#   take difference of status quo and screening active TB cases

# probability cost-effective for adherence vs uptake, for given costs per test


