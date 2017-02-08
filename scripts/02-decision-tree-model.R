#
# project: LTBI screening
# N Green
# Oct 2016
#
# LTBI screening and treatment pathway decision tree model


library(readxl)
library(data.tree)
library(treeSimR)


# output to screen
options("max.print" = 3000)


# initiate decision tree --------------------------------------------------

osNode.cost.fileName <- system.file("data", "LTBI_dtree-cost-symptoms.yaml", package = "LTBIscreeningproject")
osNode.health.fileName <- system.file("data", "LTBI_dtree-QALYloss-symptoms.yaml", package = "LTBIscreeningproject")


# deterministic parameter value grids -------------------------------------
# comment-out appropriately

# grid of parameter values for deterministic sensitivity analysis
scenario_parameter_cost <- read_excel(parameter_values_file, sheet = "cost")
scenario_parameter_p <- read_excel(parameter_values_file, sheet = "p")

## or
# baseline only
# scenario_parameter_cost <- data.frame("node" = "Agree to Screen",
#                                       "distn" = "unif",
#                                       "min" = 50,
#                                       "max" = 50,
#                                       "scenario" = 1, check.names = FALSE)
#
# scenario_parameter_p <- data.frame("Agree to Screen" = 0.5,
#                                    "Not Agree to Screen" = 0.5,
#                                    "Start Treatment" = 0.5,
#                                    "Not Start Treatment" = 0.5,
#                                    "Complete Treatment" = 0.75,
#                                    "Not Complete Treatment" = 0.25,
#                                    "scenario" = 1, check.names = FALSE)



n.scenarios <- length(unique(scenario_parameter_p$scenario))


# create decision tree objects
## cost
costeff.cost <- treeSimR::costeffectiveness_tree(yaml_tree = osNode.cost.fileName)
osNode.cost <- costeff.cost$osNode


## health
costeff.health <- treeSimR::costeffectiveness_tree(yaml_tree = osNode.health.fileName)
osNode.health <- costeff.health$osNode

# print(costeff.cost)
# print(costeff.health)



# assign cohort WHO TB incidence group branching proportions, for given year -------

for (i in seq_along(who_levels)){

  osNode.cost$Set(p = p.who_year[i],
                  filterFun = function(x) x$name==who_levels[i])

  osNode.health$Set(p = p.who_year[i],
                    filterFun = function(x) x$name==who_levels[i])
}


# assign LTBI probability to each WHO active TB prevalence group  ------------------

for (i in who_levels){

  pLTBI <- subset(pLatentTB.who_year, who_prev_cat_Pareek2011==i, select = LTBI)

  osNode.cost$Set(p = pLTBI, filterFun = function(x) x$pathString==paste("LTBI screening cost", i, "LTBI", sep="/"))
  osNode.health$Set(p = pLTBI, filterFun = function(x) x$pathString==paste("LTBI screening cost", i, "LTBI", sep="/"))

  osNode.cost$Set(p = 1 - pLTBI, filterFun = function(x) x$pathString==paste("LTBI screening cost", i, "non-LTBI", sep="/"))
  osNode.health$Set(p = 1 - pLTBI, filterFun = function(x) x$pathString==paste("LTBI screening cost", i, "non-LTBI", sep="/"))
}


# sensitivity analysis ----------------------------------------------------
# iterate over each deterministic scenario of parameter values

# delete old output files
if(file.exists(paste(diroutput, "mc_cost.csv", sep = "/"))){

  file.remove(paste(diroutput, "mc_cost.csv", sep = "/"))
}

if(file.exists(paste(diroutput, "mc_health.csv", sep = "/"))){

  file.remove(paste(diroutput, "mc_health.csv", sep = "/"))
}

if(file.exists(paste(diroutput, "prob_complete_Tx_given_LTBI_by_who.csv", sep = "/"))){

  file.remove(paste(diroutput, "prob_complete_Tx_given_LTBI_by_who.csv", sep = "/"))
}


# transform to tidy format
scenario_parameter_p.melt <- as.data.frame(scenario_parameter_p) %>%
                              reshape2::melt(id.vars = "scenario") %>%
                              plyr::rename(replace = c("variable" = "node",
                                                       "value" = "p"))



for (scenario_i in seq_len(n.scenarios)){


  print(sprintf("scenario: %d", scenario_i))


  assign_branch_values(osNode.cost,
                       osNode.health,
                       parameter_p = subset(scenario_parameter_p.melt, scenario == scenario_i),
                       parameter_cost = subset(scenario_parameter_cost, scenario == scenario_i))



  # pathway probabilities ---------------------------------------------------

  # calculate total probabilities along each branch, from root to leaf
  path_probs.screen <- treeSimR::calc_pathway_probs(osNode.cost)
  osNode.cost$Set(path_probs = path_probs.screen)

  # print(osNode.cost)



  # total prob successfully cured of LTBI for each WHO category -------------

  # number of ways to effectively compelete Tx per LTBI
  LTBItreeClone <- Clone(osNode.cost$`(50,150]`$LTBI,
                         pruneFun = function(x) myPruneFun(x, "Effective"))

  Effective.leafCount <- LTBItreeClone$leafCount

  p.complete_Tx <- osNode.cost$Get('path_probs',
                                   filterFun = function(x) x$name=="Effective")

  p.LTBI <- osNode.cost$Get('path_probs',
                                   filterFun = function(x) x$name=="LTBI")

  # sum path_probs over all leafs in WHO groups
  Effective.groups <- rep(seq_along(p.LTBI), each = Effective.leafCount)
  p.Effective <- aggregate(p.complete_Tx, by = list(Effective.groups), FUN = sum)$x

  p.complete_Tx_given_LTBI_by_who <- set_names(p.Effective/p.LTBI, nm = who_levels)



  # sample total expected values --------------------------------------------

  mc.cost <- treeSimR::MonteCarlo_expectedValues(osNode = osNode.cost, n = N.mc)
  mc.health <- treeSimR::MonteCarlo_expectedValues(osNode.health, n = N.mc)



  # save --------------------------------------------------------------------

  appcat <- pryr::partial(cat, append = TRUE, fill = TRUE)

  # cost-effectiveness outputs
  appcat(x = paste(as.numeric(mc.cost$`expected values`), collapse = ","),
         file = paste(diroutput, "mc_cost.csv", sep = "/"))

  appcat(x = paste(as.numeric(mc.health$`expected values`), collapse = ","),
         file = paste(diroutput, "mc_health.csv", sep = "/"))

  # defined pathway probabilities
  appcat(x = paste(as.numeric(p.complete_Tx_given_LTBI_by_who), collapse = ","),
         file = paste(diroutput, "prob_complete_Tx_given_LTBI_by_who.csv", sep = "/"))
}

