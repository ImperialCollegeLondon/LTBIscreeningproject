
# LTBI screening model
# N Green
# May 2017
#
# create data in a format to use in DIDE computer cluster functions
# run jobs using network drive Q:\R\cluster--LTBI-decision-tree


library(data.tree)
library(dplyr)
library(plyr)


osNode.cost.fileName <- system.file("data", "LTBI_dtree-cost-symptoms.yaml",
                                    package = "LTBIscreeningproject")

osNode.health.fileName <- system.file("data", "LTBI_dtree-QALYloss-symptoms.yaml",
                                      package = "LTBIscreeningproject")

# parameter_values_file <- system.file("data", sprintf("scenario-parameter-values_%s.xlsx", study),
#                                      package = "LTBIscreeningproject")

parameter_values_file <- system.file("data", "scenario-parameter-values.xlsx",
                                     package = "LTBIscreeningproject")

scenario_parameter_cost <- readxl::read_excel(parameter_values_file,
                                              sheet = "cost")

scenario_parameter_p <- readxl::read_excel(parameter_values_file,
                                           sheet = "p")

## cost
costeff.cost <- treeSimR::costeffectiveness_tree(yaml_tree = osNode.cost.fileName)
osNode.cost <- costeff.cost$osNode


## health
costeff.health <- treeSimR::costeffectiveness_tree(yaml_tree = osNode.health.fileName)
osNode.health <- costeff.health$osNode

who_levels <- c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]")

# 2009 cohort
##TODO: hardcoded
p_incid_grp <- c("(0,50]" = 0,
                 "(50,150]" = 0.02505289,
                 "(150,250]" = 0.09000483,
                 "(250,350]" = 0.02046914,
                 "(350,1e+05]" = 0.86447315)

# drop incidence group not screened
p_incid_grp[!names(p_incid_grp) %in% incidence_grps_screen] <- 0
p_incid_grp <- p_incid_grp/sum(p_incid_grp)

pLatentTB.who_year <- data.frame(who_prev_cat_Pareek2011 = names(p_incid_grp),
                                 LTBI = c(0.03, 0.13, 0.2, 0.3, 0.3))


# insert probs in incidence group for given year (2009) -----------------

for (i in seq_along(who_levels)) {

  osNode.cost$Set(p = p_incid_grp[i],
                  filterFun = function(x) x$name == who_levels[i])

  osNode.health$Set(p = p_incid_grp[i],
                    filterFun = function(x) x$name == who_levels[i])
}


pastef <- purrr::partial(...f = paste, sep = "/")


# insert LTBI probs -----------------------------------------------------

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


# transform probabilities to long format

scenario_parameter_p.melt <-
  as.data.frame(scenario_parameter_p) %>%
  reshape2::melt(id.vars = "scenario") %>%
  plyr::rename(replace = c("variable" = "node",
                           "value" = "p"))

# combine probs and costs in to a single array

scenario_parameter_cost$val_type <- "cost"
scenario_parameter_p.melt$val_type <- "QALYloss"

scenario_parameters <- plyr::rbind.fill(scenario_parameter_cost,
                                        scenario_parameter_p.melt)

# split by scenario to lists
scenario_parameters <- plyr::dlply(scenario_parameters, .(scenario))


#  ------------------------------------------------------------------------

data_folder <- system.file("data", package = "LTBIscreeningproject")

save(scenario_parameters, file = paste0(data_folder, "/scenario_parameters.RData"))
save(osNode.cost, file = paste0(data_folder, "/osNode_cost_2009.RData"))
save(osNode.health, file = paste0(data_folder, "/osNode_health_2009.RData"))

