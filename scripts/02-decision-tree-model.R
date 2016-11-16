#
# project: LTBI screening
# N Green
# Oct 2016
#
# screening and treatment pathway decision tree model


library(readxl)
library(data.tree)
library(treeSimR)

options("max.print"=2000)


# initiate decision tree --------------------------------------------------

##TODO##
# see if can build tree from smaller trees...

# create decision tree
## cost
osNode.cost <- treeSimR::costeffectiveness_tree(yaml_tree = "data/LTBI_dtree-cost.yaml")

## health
osNode.health <- treeSimR::costeffectiveness_tree(yaml_tree = "data/LTBI_dtree-health.yaml")

# print(osNode.cost, "type", "p", "distn", "mean", "sd", "min", "max", "a", "b", "shape", "scale", limit = NULL)
# print(osNode.health, "type", "p", "distn", "mean", "sd", "min", "max", "a", "b", "shape", "scale", limit = NULL)

# grid of parameter values for deterministic sensitivity analysis
scenario_parameter_cost <- read_excel("data/scenario-parameter-values.xlsx", sheet = "cost")
scenario_parameter_p <- read_excel("data/scenario-parameter-values.xlsx", sheet = "p")



# assign cohort WHO TB incidence group branching proportions, for given year -----------

for (i in seq_along(who_levels)){

  osNode.cost$Set(p = p.who[i],
                  filterFun = function(x) x$name==who_levels[i])

  osNode.health$Set(p = p.who[i],
                    filterFun = function(x) x$name==who_levels[i])
}
rm(i)



# sensitivity analysis ----------------------------------------------------
# iterate over each deterministic scenario of parameter values

# delete old output files
if(file.exists("ext-data/mc_cost.csv")) file.remove("ext-data/mc_cost.csv")
if(file.exists("ext-data/mc_health.csv")) file.remove("ext-data/mc_health.csv")
if(file.exists("ext-data/prob_complete_Tx_given_LTBI_by_who.csv")) file.remove("ext-data/prob_complete_Tx_given_LTBI_by_who.csv")


for (scenario in seq_len(n.scenarios)){

  print(sprintf("scenario: %d", scenario))

  # assign branching _probabilities_
  osNode.cost$Set(p = scenario_parameter_p[scenario, "Agree to Screen"],
                  filterFun = function(x) x$name=="Agree to Screen")
  osNode.cost$Set(p = scenario_parameter_p[scenario, "Not Agree to Screen"],
                  filterFun = function(x) x$name=="Not Agree to Screen")

  osNode.cost$Set(p = scenario_parameter_p[scenario, "Start Treatment"],
                  filterFun = function(x) x$name=="Start Treatment")
  osNode.cost$Set(p = scenario_parameter_p[scenario, "Not Start Treatment"],
                  filterFun = function(x) x$name=="Not Start Treatment")

  osNode.cost$Set(p = scenario_parameter_p[scenario, "Complete Treatment"],
                  filterFun = function(x) x$name=="Complete Treatment")
  osNode.cost$Set(p = scenario_parameter_p[scenario, "Not Complete Treatment"],
                  filterFun = function(x) x$name=="Not Complete Treatment")

  osNode.health$Set(p = scenario_parameter_p[scenario, "Agree to Screen"],
                    filterFun = function(x) x$name=="Agree to Screen")
  osNode.health$Set(p = scenario_parameter_p[scenario, "Not Agree to Screen"],
                    filterFun = function(x) x$name=="Not Agree to Screen")

  osNode.health$Set(p = scenario_parameter_p[scenario, "Start Treatment"],
                    filterFun = function(x) x$name=="Start Treatment")
  osNode.health$Set(p = scenario_parameter_p[scenario, "Not Start Treatment"],
                    filterFun = function(x) x$name=="Not Start Treatment")

  osNode.health$Set(p = scenario_parameter_p[scenario, "Complete Treatment"],
                    filterFun = function(x) x$name=="Complete Treatment")
  osNode.health$Set(p = scenario_parameter_p[scenario, "Not Complete Treatment"],
                    filterFun = function(x) x$name=="Not Complete Treatment")

  # assign branching _costs_
  osNode.cost$Set(distn = "unif",
                  filterFun = function(x) x$name=="Agree to Screen")
  osNode.cost$Set(min = scenario_parameter_cost[scenario, "Agree to Screen"],
                  filterFun = function(x) x$name=="Agree to Screen")
  osNode.cost$Set(max = scenario_parameter_cost[scenario, "Agree to Screen"],
                  filterFun = function(x) x$name=="Agree to Screen")


  # pathway probabilities ---------------------------------------------------

  # calculate total probabilities along each branch, from root to leaf
  ## screening
  path_probs.screen <- treeSimR::calc_pathway_probs(osNode.cost)
  osNode.cost$Set(path_probs = path_probs.screen)

  # print(osNode.cost, "type", "p", "path_probs", "distn",
  #       "mean", "sd", "min", "max", "a", "b", "shape", "scale", limit = NULL)

  ##TODO##
  ## with data.tree operations?

  # total probability successfully complete treatment of LTBI for each WHO category
  # use when know active TB cases in advance
  p.complete_Tx <- osNode.cost$Get('path_probs',
                                   filterFun = function(x) x$name=="Complete Treatment" & !grepl(pattern = "non-LTBI", x$pathString))
  p.LTBI <- osNode.cost$Get('path_probs',
                                   filterFun = function(x) x$name=="LTBI")

  # prob of completing treatment for LTBI individuals in each WHO category
  p.complete_Tx_given_LTBI_by_who <- setNames(p.complete_Tx/p.LTBI, nm = who_levels)



  # sample total expected values ---------------------------------------------------------

  mc.cost <- treeSimR::MonteCarlo_expectedValues(osNode = osNode.cost, n = N.mc)
  mc.health <- treeSimR::MonteCarlo_expectedValues(osNode.health, n = N.mc)


  ########
  # save #
  ########

  # cost-effectiveness outputs
  cat(x = paste(as.numeric(mc.cost$`expected values`), collapse=","),
      file = "ext-data/mc_cost.csv", append = TRUE, fill = TRUE)

  cat(x = paste(as.numeric(mc.health$`expected values`), collapse=","),
      file = "ext-data/mc_health.csv", append = TRUE, fill = TRUE)

  # pathway probabilities
  cat(x = paste(as.numeric(p.complete_Tx_given_LTBI_by_who), collapse=","),
      file = "ext-data/prob_complete_Tx_given_LTBI_by_who.csv", append = TRUE, fill = TRUE)
}

