#
# project: LTBI screening
# N Green
# Oct 2016
#
# screening and treatment pathway decision tree model


library(readxl)
library(data.tree)
library(treeSimR)


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



# assign cohort WHO group branching proportions, for given year -----------

for (i in seq_along(who_levels)){

  osNode.cost$Set(p = p.who[i],
                  filterFun = function(x) x$name==who_levels[i])

  osNode.health$Set(p = p.who[i],
                    filterFun = function(x) x$name==who_levels[i])
}
rm(i)


# number of Monte Carlo iterations
N.mc <- 10
n.scenarios <- nrow(scenario_parameter_cost)


# sensitivity analysis ----------------------------------------------------
# iterate over each deterministic scenario of parameter values

# delete old output files
if(file.exists("ext-data/mc_cost.csv")) file.remove("ext-data/mc_cost.csv")
if(file.exists("ext-data/mc_health.csv")) file.remove("ext-data/mc_health.csv")
if(file.exists("ext-data/prob_complete_Tx_given_LTBI_by_who.csv")) file.remove("ext-data/prob_complete_Tx_given_LTBI_by_who.csv")


for (scenario in 1:10){ #seq_len(n.scenarios)){

  print(sprintf("scenario: %d", scenario))

  # assign branching _probabilities_
  osNode.cost$Set(p = scenario_parameter_p[scenario, "Screening"],
                  filterFun = function(x) x$name=="Screening")
  osNode.cost$Set(p = scenario_parameter_p[scenario, "No Screening"],
                  filterFun = function(x) x$name=="No Screening")

  osNode.cost$Set(p = scenario_parameter_p[scenario, "Start Treatment"],
                  filterFun = function(x) x$name=="Start Treatment")
  osNode.cost$Set(p = scenario_parameter_p[scenario, "Not Start Treatment"],
                  filterFun = function(x) x$name=="Not Start Treatment")

  osNode.cost$Set(p = scenario_parameter_p[scenario, "Complete Treatment"],
                  filterFun = function(x) x$name=="Complete Treatment")
  osNode.cost$Set(p = scenario_parameter_p[scenario, "Not Complete Treatment"],
                  filterFun = function(x) x$name=="Not Complete Treatment")

  osNode.health$Set(p = scenario_parameter_p[scenario, "Screening"],
                    filterFun = function(x) x$name=="Screening")
  osNode.health$Set(p = scenario_parameter_p[scenario, "No Screening"],
                    filterFun = function(x) x$name=="No Screening")

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

  # test sum to 1
  sum(osNode.cost$Get("path_probs", filterFun = isLeaf))

  ##TODO##
  ## this is a bit messy because the screening (uncertain) event is in between the WHO and LTBI events on the tree
  ## and this may be different for different groups
  ## theres probably a better way to do this:
  ##    with data.tree operations?
  ##    re-order tree structure?

  # total probability successfully complete treatment of LTBI
  # use when know active TB cases in advance
  p.complete_Tx <- osNode.cost$Get('path_probs',
                                   filterFun = function(x) x$name=="Complete Treatment" & !grepl(pattern = "non-LTBI", x$pathString))

  # branch probabilities for LTBI
  p.LTBI <- osNode.cost$Get('p',
                            filterFun = function(x) x$name=="LTBI" & !grepl(pattern = "No Screening", x$pathString))

  # prob of completing treatment for LTBI individuals in each WHO category
  p.complete_Tx_given_LTBI_by_who <- setNames(p.complete_Tx/(p.LTBI * p.who), nm = who_levels)



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

