#*******************************************************************
# LTBI screening model
# N Green
# May 2017
#
# create data in a format for parallel batch runs
# e.g. use in DIDE computer cluster functions
# run jobs using network drive Q:\R\cluster--LTBI-decision-tree


# load input files ---------------------------------------------------------

osNode.cost.fileName <- system.file("data", "LTBI_dectree-cost.yaml",
                                    package = "LTBIscreeningproject")

osNode.health.fileName <- system.file("data", "LTBI_dectree-QALY.yaml",
                                      package = "LTBIscreeningproject")

## cost
costeff.cost <- treeSimR::costeffectiveness_tree(yaml_tree = osNode.cost.fileName)
osNode.cost <- costeff.cost$osNode

## health
costeff.health <- treeSimR::costeffectiveness_tree(yaml_tree = osNode.health.fileName)
osNode.health <- costeff.health$osNode

who_levels <-
  osNode.cost$Get('name',
                  pruneFun = function(x) x$level <= 2)[-1] %>%
  'names<-'(NULL)

p_incid_grp <- factor(cohort$who_inc_Pareek2011,
                      levels = who_levels) %>% prop_table()

##TODO: this is a duplicated elsewhere
pLatentTB.who <- data.frame(who_inc_Pareek2011 = who_levels,
                            LTBI = c(0.03, 0.13, 0.2, 0.3, 0.27))


# insert probs in incidence group for given year (2009) -----------------

for (i in seq_along(p_incid_grp)) {

  osNode.cost$Set(pmin = p_incid_grp[i],
                  pmax = p_incid_grp[i],
                  filterFun = function(x) x$name == names(p_incid_grp)[i])

  osNode.health$Set(pmin = p_incid_grp[i],
                    pmax = p_incid_grp[i],
                    filterFun = function(x) x$name == names(p_incid_grp)[i])
}


# insert LTBI probs -----------------------------------------------------

for (i in who_levels) {

  pLTBI <- subset(pLatentTB.who,
                  who_inc_Pareek2011 == i,
                  select = LTBI)

  osNode.cost$Set(pmin = pLTBI,
                  pmax = pLTBI,
                  filterFun = function(x) x$pathString == pastef("LTBI screening cost", i, "LTBI"))

  osNode.health$Set(pmin = pLTBI,
                    pmax = pLTBI,
                    filterFun = function(x) x$pathString == pastef("LTBI screening QALY loss", i, "LTBI"))
}


# treatment ---------------------------------------------------------------

## cost
cost <- unit_cost[[interv$treatment]]

osNode.cost$Set(min = cost$full,
                max = cost$full,
                filterFun = function(x) x$name == "Complete Treatment")

osNode.cost$Set(min = cost$dropout,
                max = cost$dropout,
                filterFun = function(x) x$name == "Not Complete Treatment")

## effectiveness

eff <- effectiveness[[interv$treatment]]

osNode.cost$Set(pmin = eff$pmin,
                filterFun = function(x) x$name == "Effective")

osNode.cost$Set(pmax = eff$pmax,
                filterFun = function(x) x$name == "Effective")

osNode.health$Set(pmin = eff$pmin,
                  filterFun = function(x) x$name == "Effective")

osNode.health$Set(pmax = eff$pmax,
                  filterFun = function(x) x$name == "Effective")


# LTBI test prob -----------------------------------------------------------

performance <-  test_performance[[interv$LTBI_test]]

# cost

osNode.cost$Set(pmin = performance$sens$pmin,
                filterFun = function(x) x$name == "Sensitivity")

osNode.cost$Set(pmax = performance$sens$pmax,
                filterFun = function(x) x$name == "Sensitivity")

osNode.cost$Set(pmin = performance$spec$pmin,
                filterFun = function(x) x$name == "Specificity")

osNode.cost$Set(pmax = performance$spec$pmax,
                filterFun = function(x) x$name == "Specificity")

# health

osNode.health$Set(pmin = performance$sens$pmin,
                  filterFun = function(x) x$name == "Sensitivity")

osNode.health$Set(pmax = performance$sens$pmax,
                  filterFun = function(x) x$name == "Sensitivity")

osNode.health$Set(pmin = performance$spec$pmin,
                  filterFun = function(x) x$name == "Specificity")

osNode.health$Set(pmax = performance$spec$pmax,
                  filterFun = function(x) x$name == "Specificity")


# GP incentives -----------------------------------------------------------

ltbi_positive <- unit_cost$GP_incentive$ltbi_positive

osNode.cost$Set(min = ltbi_positive$params['mean'],
                max = ltbi_positive$params['mean'],
                filterFun = function(x) x$name == "Sensitivity")

osNode.cost$Set(min = ltbi_positive$params['mean'],
                max = ltbi_positive$params['mean'],
                filterFun = function(x) x$name == "1-Specificity")


#  save -----------------------------------------------------------------

data_folder <- system.file("data", package = "LTBIscreeningproject")

save(osNode.cost, file = paste0(data_folder, "/osNode_cost_2009.RData"))
save(osNode.health, file = paste0(data_folder, "/osNode_health_2009.RData"))

saveRDS(osNode.cost, file = paste0("Q:/R/cluster--LTBI-decision-tree", "/osNode_cost_2009.Rds"))
saveRDS(osNode.health, file = paste0("Q:/R/cluster--LTBI-decision-tree", "/osNode_health_2009.Rds"))

