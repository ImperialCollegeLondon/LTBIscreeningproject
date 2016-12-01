
library(dplyr)

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


## not standard way of doing this
# this is on each simulation and not the scenario averages
# INMB
total_diff$INMB20000 <- INMB(total_diff$health_diff, total_diff$cost_diff, 20000)
total_diff$INMB30000 <- INMB(total_diff$health_diff, total_diff$cost_diff, 30000)

# ICER
total_diff$ICER <- ICER(total_diff$health_diff, total_diff$cost_diff)





total_diff %>%
  group_by(scenario) %>%
  summarise(E.screen.cost = mean(value.screen.cost),
            E.screen.health = mean(value.screen.health))

E.cost.screened <- E.screen.cost + E.aTB_cost.screened
E.QALY.screened <- E.screen.health + E.aTB_QALY.screened

E.cost.statusquo
E.QALY.statusquo

INMB(E.QALY.screened - E.QALY.statusquo,
     E.cost.screened - E.cost.statusquo,
     20000)

INMB(E.QALY.screened - E.QALY.statusquo,
     E.cost.screened - E.cost.statusquo,
     30000)

ICER(E.QALY.screened - E.QALY.statusquo,
     E.cost.screened - E.cost.statusquo)



# CEAC


# prob INMB>0






