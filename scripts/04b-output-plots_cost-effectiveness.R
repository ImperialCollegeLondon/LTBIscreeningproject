#
# project: LTBI screening
# N Green
# Oct 2016
#
# output cost-effectiveness plots


library(ggplot2)
library(BCEA)


i <- 1 #uk_tbX

# single intervention
e.total <- matrix(c(rep(0,N.mc), -mc.health$`expected values`), #) + aTB_QALYloss[i, scenario]),
                  ncol = 2, dimnames = list(NULL, c("do nothing","interv")))

c.total <- matrix(c(rep(0,N.mc), mc.cost$`expected values`), # + aTB_cost[i, scenario])
                  ncol = 2, dimnames = list(NULL, c("do nothing","interv")))


# multiple scenarios intervention
mc_cost_scenarios <- read.csv(file = "ext-data/mc_cost.csv", header = FALSE)
mc_health_scenarios <- read.csv(file = "ext-data/mc_health.csv", header = FALSE)

c.total <- t(rbind(0, mc_cost_scenarios))
colnames(c.total) <- as.character(0:nrow(mc_cost_scenarios))



##TODO##
# if n.uk_tbX==n.mc
# then can do a 1-to-1 pairing off below
# for now just use same value (not very variable anyway)

# add active TB costs
c.total <- mapply(c.total, aTB_cost[1, ], FUN = function(x,y) x+y)

e.total <- t(rbind(0, -mc_health_scenarios))
colnames(e.total) <- as.character(0:nrow(mc_health_scenarios))

# add active TB QALY loss
e.total <- mapply(e.total, aTB_QALYloss[1, ], FUN = function(x,y) x+y)


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


