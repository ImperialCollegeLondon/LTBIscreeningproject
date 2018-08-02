
#' ---
#' title: "LTBI screening model:
#' prep decision tree with scenario data for parallel batch runs"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---
#
# e.g. use in DIDE computer cluster functions
# run jobs using network drive Q:\R\cluster--LTBI-decision-tree


# load input files ---------------------------------------------------------

costeff.cost <-
  costeffectiveness_tree(yaml_tree = here::here("data", "LTBI_dectree-cost.yaml"))
osNode.cost <- costeff.cost$osNode

costeff.health <-
  costeffectiveness_tree(yaml_tree = here::here("data", "LTBI_dectree-QALY.yaml"))
osNode.health <- costeff.health$osNode

who_levels <-
  osNode.cost$Get('name',
                  pruneFun = function(x) x$level <= 2)[-1] %>%
  'names<-'(NULL)

p_incid_grp <- factor(cohort$who_inc_Pareek2011,
                      levels = who_levels) %>% prop_table()

pLatentTB.who <-
  aggregate(cohort$LTBI,
            by = list(cohort$who_inc_Pareek2011),
            function(x) mean(as.numeric(x), na.rm = TRUE)) %>%
  setNames(c("who_inc_Pareek2011", "LTBI"))

# fill in missing levels
pLatentTB.who <-
  pLatentTB.who %>%
  right_join(data.frame(who_inc_Pareek2011 = who_levels),
            by = "who_inc_Pareek2011") %>%
  mutate(LTBI = ifelse(is.na(LTBI), 0, LTBI))

# should be close to
# data.frame(who_inc_Pareek2011 = who_levels, LTBI = c(0.03, 0.13, 0.2, 0.3, 0.27))


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

set_branch_uniform_params(eff, osNode.cost)
set_branch_uniform_params(eff, osNode.health)


# LTBI test prob -----------------------------------------------------------

performance <-  test_performance[[interv$LTBI_test]]

set_branch_uniform_params(performance, osNode.cost)
set_branch_uniform_params(performance, osNode.health)


# GP incentives -----------------------------------------------------------

ltbi_positive <- unit_cost$GP_incentive$ltbi_positive

osNode.cost$Set(min = ltbi_positive$params['mean'],
                max = ltbi_positive$params['mean'],
                filterFun = function(x) x$name == "Sensitivity")

osNode.cost$Set(min = ltbi_positive$params['mean'],
                max = ltbi_positive$params['mean'],
                filterFun = function(x) x$name == "1-Specificity")


#  save -----------------------------------------------------------------

save(osNode.cost, file = here::here("data", "osNode_cost_2009.RData"))
save(osNode.health, file = here::here("data", "osNode_health_2009.RData"))

saveRDS(osNode.cost, file = paste0("Q:/R/cluster--LTBI-decision-tree", "/osNode_cost_2009.Rds"))
saveRDS(osNode.health, file = paste0("Q:/R/cluster--LTBI-decision-tree", "/osNode_health_2009.Rds"))
