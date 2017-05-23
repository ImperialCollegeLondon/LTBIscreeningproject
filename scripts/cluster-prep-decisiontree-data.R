
#create data in a format to use in cluster functions


osNode.cost.fileName <- system.file("data", "LTBI_dtree-cost-symptoms.yaml",
                                    package = "LTBIscreeningproject")

osNode.health.fileName <- system.file("data", "LTBI_dtree-QALYloss-symptoms.yaml",
                                      package = "LTBIscreeningproject")


scenario_parameter_cost <- read_excel(parameter_values_file,
                                      sheet = "cost")

scenario_parameter_p <- read_excel(parameter_values_file,
                                   sheet = "p")

## cost
costeff.cost <- treeSimR::costeffectiveness_tree(yaml_tree = osNode.cost.fileName)
osNode.cost <- costeff.cost$osNode


## health
costeff.health <- treeSimR::costeffectiveness_tree(yaml_tree = osNode.health.fileName)
osNode.health <- costeff.health$osNode


for (i in seq_along(who_levels)) {

  osNode.cost$Set(p = p.who_year[i],
                  filterFun = function(x) x$name == who_levels[i])

  osNode.health$Set(p = p.who_year[i],
                    filterFun = function(x) x$name == who_levels[i])
}


for (i in who_levels) {

  pLTBI <- subset(pLatentTB.who_year,
                  who_prev_cat_Pareek2011 == i,
                  select = LTBI)

  osNode.cost$Set(p = pLTBI,
                  filterFun = function(x) x$pathString == pastef("LTBI screening cost", i, "LTBI"))
  osNode.health$Set(p = pLTBI,
                    filterFun = function(x) x$pathString == pastef("LTBI screening cost", i, "LTBI"))

  osNode.cost$Set(p = 1 - pLTBI,
                  filterFun = function(x) x$pathString == pastef("LTBI screening cost", i, "non-LTBI"))
  osNode.health$Set(p = 1 - pLTBI,
                    filterFun = function(x) x$pathString == pastef("LTBI screening cost", i, "non-LTBI"))
}

scenario_parameter_p.melt <-
  as.data.frame(scenario_parameter_p) %>%
  reshape2::melt(id.vars = "scenario") %>%
  plyr::rename(replace = c("variable" = "node",
                           "value" = "p"))


scenario_parameter_cost$val_type <- "cost"
scenario_parameter_p.melt$val_type <- "QALYloss"
scenario_parameters <- rbind.fill(scenario_parameter_cost, scenario_parameter_p.melt)

scenario_parameters <- dlply(scenario_parameters, .(scenario))


#  ------------------------------------------------------------------------

save(scenario_parameters, file = "C:/Users/ngreen1/Dropbox/TB/LTBI/R/LTBIscreeningproject/data/scenario_parameters.RData")
save(osNode.cost, file = "C:/Users/ngreen1/Dropbox/TB/LTBI/R/LTBIscreeningproject/data/osNode_cost_2009.RData")
save(osNode.health, file = "C:/Users/ngreen1/Dropbox/TB/LTBI/R/LTBIscreeningproject/data/osNode_health_2009.RData")

