##to debug
# res <- lapply(scenario_parameters,
#               decision_tree_cluster,
#               N.mc = N.mc,
#               n.uk_tb = n.uk_tb,
#               n.exit_tb = n.exit_tb)
#
# xx <- decision_tree_cluster(parameters = scenario_parameters[[1]],
#                             n.uk_tb = 10,
#                             n.exit_tb = 10,
#                             cost_dectree = "osNode_cost_2009.Rds",
#                             health_dectree = "osNode_health_2009.Rds")
#
# xx <- decision_tree_cluster(parameters = scenario_parameters[[1]][1:3, ],
#                             n.uk_tb = 10,
#                             n.exit_tb = 10,
#                             cost_dectree = "osNode_cost_2009_pdistn.Rds",
#                             health_dectree = "osNode_health_2009_pdistn.Rds")
