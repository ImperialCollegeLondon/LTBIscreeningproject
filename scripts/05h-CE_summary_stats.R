#
# project: LTBI screening
# N Green
# Nov 2016
#
# calculate cost-effectiveness statistics for results tables


library(dplyr)


# create a single 'long/tidy' output array

aTB_cost_diff.melt$scenario <- seq_len(nrow(aTB_cost_diff.melt))
aTB_cost_diff.melt <- tidyr::gather(aTB_cost_diff.melt, key = MCrun, value = value, -scenario)
aTB_cost_diff.melt$MCrun <- as.numeric(as.factor(aTB_cost_diff.melt$MCrun))

aTB_QALYgain.melt$scenario <- seq_len(nrow(aTB_QALYgain.melt))
aTB_QALYgain.melt <- tidyr::gather(aTB_QALYgain.melt, key = MCrun, value = value, -scenario)
aTB_QALYgain.melt$MCrun <- as.numeric(as.factor(aTB_QALYgain.melt$MCrun))


mc_cost_scenarios$scenario <- seq_len(nrow(mc_cost_scenarios))
mc_cost_scenarios <- tidyr::gather(mc_cost_scenarios, key = MCrun, value = value, -scenario)
mc_cost_scenarios$MCrun <- as.numeric(as.factor(mc_cost_scenarios$MCrun))

mc_health_scenarios$scenario <- seq_len(nrow(mc_health_scenarios))
mc_health_scenarios <- tidyr::gather(mc_health_scenarios, key = MCrun, value = value, -scenario)
mc_health_scenarios$MCrun <- as.numeric(as.factor(mc_health_scenarios$MCrun))


cost_diff <- dplyr::full_join(aTB_cost_diff.melt, mc_cost_scenarios, by = c("scenario", "MCrun"), suffix = c(".aTB", ".screen"))
cost_diff <- transform(cost_diff, cost_diff = value.aTB + value.screen)

health_diff <- dplyr::full_join(aTB_QALYgain.melt, mc_health_scenarios, by = c("scenario", "MCrun"), suffix = c(".aTB", ".screen"))
health_diff <- transform(health_diff, health_diff = value.aTB + value.screen)

total_diff <- dplyr::full_join(health_diff, cost_diff, by = c("scenario", "MCrun"), suffix = c(".health", ".cost"))



total_diff.means <-
  total_diff %>%
  group_by(scenario) %>%
  summarise(E.screen.cost = mean(value.screen.cost),
            E.screen.health = mean(value.screen.health))

E.cost.screened <- total_diff.means$E.screen.cost + E.aTB_cost.screened
E.QALY.screened <- total_diff.means$E.screen.health + E.aTB_QALY.screened


total_diff.mean$INMB20000 <- INMB(E.QALY.screened - E.aTB_QALY.statusquo,
                                  E.cost.screened - E.aTB_cost.statusquo,
                                  20000)

total_diff.mean$INMB30000 <- INMB(E.QALY.screened - E.aTB_QALY.statusquo,
                                  E.cost.screened - E.aTB_cost.statusquo,
                                  30000)

total_diff.mean$ICER <- ICER(E.QALY.screened - E.aTB_QALY.statusquo,
                             E.cost.screened - E.aTB_cost.statusquo)


#  ------------------------------------------------------------------------

## not standard way of doing this
# this is on each simulation and not the scenario averages
# INMB
total_diff$INMB20000 <- INMB(total_diff$health_diff, total_diff$cost_diff, 20000)
total_diff$INMB30000 <- INMB(total_diff$health_diff, total_diff$cost_diff, 30000)
# ICER
total_diff$ICER <- ICER(total_diff$health_diff, total_diff$cost_diff)

# prob INMB>0

total_diff %>%
  group_by(scenario) %>%
  summarise(sum(INMB20000>0)/length(INMB20000))

total_diff %>%
  group_by(scenario) %>%
  summarise(sum(INMB30000>0)/length(INMB30000))


##TODO: CEAC table





