#
# project: LTBI screening
# N Green
# Oct 2016
#
# screening and treatment pathway decision tree model


library(data.tree)
library(treeSimR)


# initiate decision tree --------------------------------------------------

# create decision tree
## cost
osNode.cost <- treeSimR::costeffectiveness_tree(yaml_tree = "data/LTBI_dtree-cost.yaml")
## health
osNode.health <- treeSimR::costeffectiveness_tree(yaml_tree = "data/LTBI_dtree-health.yaml")

# print(osNode.cost, "type", "p", "distn", "mean", "sd")


# assign cohort WHO group proportions for given year
who_levels <- names(entryCohort_who_prop[[year_cohort]])

for (i in seq_along(who_levels)){

  osNode.cost$Set(p = entryCohort_who_prop[[year_cohort]][i],
                  filterFun = function(x) x$name==who_levels[i])

  osNode.health$Set(p = entryCohort_who_prop[[year_cohort]][i],
                    filterFun = function(x) x$name==who_levels[i])
}

osNode.noscreen <- osNode.cost


# intervention scenarios --------------------------------------------------

# no screening
osNode.noscreen$Set(p = 0, filterFun = function(x) x$name=="Screening")
osNode.noscreen$Set(p = 1, filterFun = function(x) x$name=="No Screening")

# screen everyone
osNode.cost$Set(p = 1, filterFun = function(x) x$name=="Screening")
osNode.cost$Set(p = 0, filterFun = function(x) x$name=="No Screening")
osNode.health$Set(p = 1, filterFun = function(x) x$name=="Screening")
osNode.health$Set(p = 0, filterFun = function(x) x$name=="No Screening")

# # screen >350/100,000 incidence only
# osNode.cost$Set(p = 0, filterFun = function(x) x$name=="Screening")
# osNode.cost$Set(p = 1, filterFun = function(x) x$name=="No Screening")
# osNode.cost$Set(p = 1, filterFun = function(x) x$pathString=="LTBI screening cost/(350, 1e+05]/Screening")
# osNode.cost$Set(p = 0, filterFun = function(x) x$pathString=="LTBI screening cost/(350, 1e+05]/No Screening")



# pathway probabilities ---------------------------------------------------

# calculate probabilitites along each branch, from root to leaf
## screening
path_probs <- treeSimR::calc_pathway_probs(osNode.cost)
osNode.cost$Set(path_probs = path_probs)
terminal_states <- data.frame(pathname = osNode.cost$Get('pathString', filterFun = isLeaf),
                              path_probs = osNode.cost$Get('path_probs', filterFun = isLeaf))

## no screening
path_probs.noscreen <- treeSimR::calc_pathway_probs(osNode.noscreen)
osNode.noscreen$Set(path_probs = path_probs.noscreen)
terminal_states.noscreen <- data.frame(pathname = osNode.noscreen$Get('pathString', filterFun = isLeaf),
                                       path_probs = osNode.noscreen$Get('path_probs', filterFun = isLeaf))


# collect final decision tree states in to
# competing risks model starting state groups

startstate.nonLTBI <- grepl("/Complete Treatment", x = terminal_states$pathname) | grepl("nonLTBI", x = terminal_states$pathname)
startstate.LTBI <- !startstate.nonLTBI

healthstatus <- NA
healthstatus[startstate.nonLTBI] <- "nonLTBI"
healthstatus[startstate.LTBI] <- "LTBI"

# sample the subpopulation sizes at each terminal node and
# aggregate to competing risk model start states

n.startstate <- 2

start_state_proportions.screen <- treeSimR::get_start_state_proportions(path_probs = terminal_states$path_probs,
                                                                        startstateid = healthstatus,
                                                                        samplesize = entryCohort_poptotal$pop[entryCohort_poptotal$year==year_cohort],
                                                                        numsamples = n.startstate)

start_state_proportions.noscreen <- treeSimR::get_start_state_proportions(path_probs = terminal_states.noscreen$path_probs,
                                                                          startstateid = healthstatus,
                                                                          samplesize = entryCohort_poptotal$pop[entryCohort_poptotal$year==year_cohort],
                                                                          numsamples = n.startstate)


# expected values ---------------------------------------------------------

N.mc <- 10

# for defined nodes, sample of expected cost realisations
mc.cost <- treeSimR::MonteCarlo_expectedValues(osNode.cost, n = N.mc)

# for defined nodes, sample of expected health realisations
mc.health <- treeSimR::MonteCarlo_expectedValues(osNode.health, n = N.mc)


