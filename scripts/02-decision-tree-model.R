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

# print(osNode.cost, "type", "p", "distn", "mean", "sd", limit = NULL)


# grid of parameter values for deterministic sensitivity analysis
scenario_parameters_cost <- read_excel("data/scenario-parameter-values.xlsx", sheet = "cost")
scenario_parameter_p <- read_excel("data/scenario-parameter-values.xlsx", sheet = "p")


# assign cohort WHO group branching proportions, for given year
who_levels <- names(entryCohort_who_prop[[year_cohort]])

for (i in seq_along(who_levels)){

  osNode.cost$Set(p = entryCohort_who_prop[[year_cohort]][i],
                  filterFun = function(x) x$name==who_levels[i])

  osNode.health$Set(p = entryCohort_who_prop[[year_cohort]][i],
                    filterFun = function(x) x$name==who_levels[i])
}


# sensitivity analysis ----------------------------------------------------

# probabilities of events
osNode.cost$Set(p = scenario_parameter_p[j, "Screening"],
                filterFun = function(x) x$name=="Screening")
osNode.cost$Set(p = scenario_parameter_p[j, "No Screening"],
                filterFun = function(x) x$name=="No Screening")

osNode.cost$Set(p = scenario_parameter_p[j, "Start Treatment"],
                filterFun = function(x) x$name=="Start Treatment")
osNode.cost$Set(p = scenario_parameter_p[j, "Not Start Treatment"],
                filterFun = function(x) x$name=="Not Start Treatment")

osNode.cost$Set(p = scenario_parameter_p[j, "Complete Treatment"],
                filterFun = function(x) x$name=="Complete Treatment")
osNode.cost$Set(p = scenario_parameter_p[j, "Not Complete Treatment"],
                filterFun = function(x) x$name=="Not Complete Treatment")

osNode.health$Set(p = scenario_parameter_p[j, "Screening"],
                  filterFun = function(x) x$name=="Screening")
osNode.health$Set(p = scenario_parameter_p[j, "No Screening"],
                  filterFun = function(x) x$name=="No Screening")

osNode.health$Set(p = scenario_parameter_p[j, "Start Treatment"],
                  filterFun = function(x) x$name=="Start Treatment")
osNode.health$Set(p = scenario_parameter_p[j, "Not Start Treatment"],
                  filterFun = function(x) x$name=="Not Start Treatment")

osNode.health$Set(p = scenario_parameter_p[j, "Complete Treatment"],
                  filterFun = function(x) x$name=="Complete Treatment")
osNode.health$Set(p = scenario_parameter_p[j, "Not Complete Treatment"],
                filterFun = function(x) x$name=="Not Complete Treatment")

# costs
osNode.cost$Set(distn = "unif",
                filterFun = function(x) x$name=="Agree to Screen")
osNode.cost$Set(min = scenario_parameter_cost[j, "Agree to Screen"],
                filterFun = function(x) x$name=="Agree to Screen")
osNode.cost$Set(max = scenario_parameter_cost[j, "Agree to Screen"],
                filterFun = function(x) x$name=="Agree to Screen")


# pathway probabilities ---------------------------------------------------

# calculate total probabilities along each branch, from root to leaf
## screening
path_probs.screen <- treeSimR::calc_pathway_probs(osNode.cost)
osNode.cost$Set(path_probs = path_probs.screen)


# probability successfully complete treatment of LTBI
# use when know active TB cases in advance
p.complete_treatment <- osNode.cost$Get('path_probs', filterFun = function(x) x$name=="Complete Treatment")
p.LTBI <- osNode.cost$Get('path_probs', filterFun = function(x) x$name=="LTBI")
p.LTBI_to_nonLTBI <- round(p.complete_treatment/p.LTBI, digits = 4)
p.LTBI_to_nonLTBI <- unique(p.LTBI_to_nonLTBI[!p.LTBI_to_nonLTBI%in%c(NA,NaN,Inf)])


# expected values ---------------------------------------------------------

N.mc <- 10

# for defined nodes, sample expected cost
mc.cost <- treeSimR::MonteCarlo_expectedValues(osNode.cost, n = N.mc)

# for defined nodes, sample expected health
mc.health <- treeSimR::MonteCarlo_expectedValues(osNode.health, n = N.mc)


