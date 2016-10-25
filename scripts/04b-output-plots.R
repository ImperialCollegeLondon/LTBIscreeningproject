#
# project: LTBI screening
# N Green
# Oct 2016
#
# output plots screening decision tree


library(BCEA)


screen.bcea <- bcea(e = cbind('donothing'=0, 'interv'=mc.health$`expected values`),
                    c = cbind('donothing'=0, 'interv'=mc.cost$`expected values`),
                    ref = 1)

ceplane.plot(screen.bcea)


##TODO##
# have number of active TB cases as health detriment instead?




##TODO##

# money saved/cases averted over time
#   take difference of status quo and screening active TB cases

# probability cost-effective for adherence vs uptake, for given costs per test


