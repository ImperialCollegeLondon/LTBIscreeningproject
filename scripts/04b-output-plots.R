#
# project: LTBI screening
# N Green
# Oct 2016
#
# output plots screening decision tree


library(ggplot2)
library(BCEA)


screen.bcea <- bcea(e = matrix(c(rep(0,N.mc), mc.health$`expected values`)), ncol=2, dimnames = list(,c("hjk;","hjk")),
                    c = cbind("donothing"=rep(0,N.mc), "interv"=mc.cost$`expected values`),
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


