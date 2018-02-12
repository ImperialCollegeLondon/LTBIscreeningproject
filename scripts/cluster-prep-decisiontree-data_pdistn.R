#*******************************************************************
# LTBI screening model
# N Green
# May 2017
#
# create data in a format to use in DIDE computer cluster functions
# run jobs using network drive Q:\R\cluster--LTBI-decision-tree


# load input files ---------------------------------------------------------

osNode.cost_pdistn.fileName <- system.file("data", "LTBI_dtree-cost-symptoms-pdistn.yaml",
                                    package = "LTBIscreeningproject")

osNode.health_pdistn.fileName <- system.file("data", "LTBI_dtree-QALYloss-symptoms-pdistn.yaml",
                                      package = "LTBIscreeningproject")

## cost
costeff.cost <- treeSimR::costeffectiveness_tree(yaml_tree = osNode.cost_pdistn.fileName)
osNode.cost_pdistn <- costeff.cost$osNode

## health
costeff.health <- treeSimR::costeffectiveness_tree(yaml_tree = osNode.health_pdistn.fileName)
osNode.health_pdistn <- costeff.health$osNode

who_levels <- c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]")

p_incid_grp <- prop_table(IMPUTED_sample_year_cohort$who_prev_cat_Pareek2011)

pLatentTB.who <- data.frame(who_prev_cat_Pareek2011 = names(p_incid_grp),
                            LTBI = c(0.03, 0.13, 0.2, 0.3, 0.3))


# insert probs in incidence group for given year (2009) -----------------

for (i in seq_along(who_levels)) {

  osNode.cost_pdistn$Set(pmin = p_incid_grp[i],
                  filterFun = function(x) x$name == who_levels[i])

  osNode.health_pdistn$Set(pmin = p_incid_grp[i],
                    filterFun = function(x) x$name == who_levels[i])

  osNode.cost_pdistn$Set(pmax = p_incid_grp[i],
                  filterFun = function(x) x$name == who_levels[i])

  osNode.health_pdistn$Set(pmax = p_incid_grp[i],
                    filterFun = function(x) x$name == who_levels[i])
}


# insert LTBI probs -----------------------------------------------------

for (i in who_levels) {

  pLTBI <- subset(pLatentTB.who,
                  who_prev_cat_Pareek2011 == i,
                  select = LTBI)

  osNode.cost_pdistn$Set(pmin = pLTBI,
                  filterFun = function(x) x$pathString == pastef("LTBI screening cost", i, "LTBI"))
  osNode.health_pdistn$Set(pmin = pLTBI,
                    filterFun = function(x) x$pathString == pastef("LTBI screening QALY loss", i, "LTBI"))

  osNode.cost_pdistn$Set(pmin = 1 - pLTBI,
                  filterFun = function(x) x$pathString == pastef("LTBI screening cost", i, "non-LTBI"))
  osNode.health_pdistn$Set(pmin = 1 - pLTBI,
                    filterFun = function(x) x$pathString == pastef("LTBI screening QALY loss", i, "non-LTBI"))

  osNode.cost_pdistn$Set(pmax = pLTBI,
                  filterFun = function(x) x$pathString == pastef("LTBI screening cost", i, "LTBI"))
  osNode.health_pdistn$Set(pmax = pLTBI,
                    filterFun = function(x) x$pathString == pastef("LTBI screening QALY loss", i, "LTBI"))

  osNode.cost_pdistn$Set(pmax = 1 - pLTBI,
                  filterFun = function(x) x$pathString == pastef("LTBI screening cost", i, "non-LTBI"))
  osNode.health_pdistn$Set(pmax = 1 - pLTBI,
                    filterFun = function(x) x$pathString == pastef("LTBI screening QALY loss", i, "non-LTBI"))
}


#  save -----------------------------------------------------------------

data_folder <- system.file("data", package = "LTBIscreeningproject")

save(osNode.cost_pdistn, file = paste0(data_folder, "/osNode_cost_2009_pdistn.RData"))
save(osNode.health_pdistn, file = paste0(data_folder, "/osNode_health_2009_pdistn.RData"))

saveRDS(osNode.cost_pdistn, file = paste0("Q:/R/cluster--LTBI-decision-tree", "/osNode_cost_2009_pdistn.Rds"))
saveRDS(osNode.health_pdistn, file = paste0("Q:/R/cluster--LTBI-decision-tree", "/osNode_health_2009_pdistn.Rds"))
