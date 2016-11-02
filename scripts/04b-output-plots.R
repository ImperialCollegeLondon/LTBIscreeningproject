#
# project: LTBI screening
# N Green
# Oct 2016
#
# output plots screening decision tree


library(ggplot2)
library(BCEA)

e.total <- matrix(c(rep(0,N.mc), mc.health$`expected values` + aTB_QALYloss[1]),
                  ncol=2, dimnames = list(NULL, c("donothing","interv")))
c.total <- cbind("donothing"=rep(0,N.mc), "interv"=mc.cost$`expected values` + aTB_cost[1])

screen.bcea <- bcea(e = e.total,
                    c = c.total,
                    ref = 1)

ceplane.plot(screen.bcea)
contour(screen.bcea)
contour2(screen.bcea, graph="ggplot2")
eib.plot(screen.bcea)
ceac.plot(screen.bcea)


##TODO##
# have number of active TB cases as health detriment instead?




##TODO##

# money saved/cases averted over time
#   take difference of status quo and screening active TB cases

# probability cost-effective for adherence vs uptake, for given costs per test


