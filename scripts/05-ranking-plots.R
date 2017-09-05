# ************************************
# LTBI screening
# N Green
# Sep 2017
#
# multiple comparisons/ranking


ce_sim_table <- sim.table(screen.bcea, wtp = 20000)

multice <- multi.ce(screen.bcea)

mce.plot(multice)
