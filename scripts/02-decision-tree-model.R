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


# intervention scenarios --------------------------------------------------

# screen everyone
osNode.cost$Set(p = 1, filterFun = function(x) x$name=="Screening")
osNode.cost$Set(p = 0, filterFun = function(x) x$name=="No Screening")

# # screen >350/100,000 incidence only
# osNode.cost$Set(p = 0, filterFun = function(x) x$name=="Screening")
# osNode.cost$Set(p = 1, filterFun = function(x) x$name=="No Screening")
# osNode.cost$Set(p = 1, filterFun = function(x) x$pathString=="LTBI screening cost/(350, 1e+05]/Screening")
# osNode.cost$Set(p = 0, filterFun = function(x) x$pathString=="LTBI screening cost/(350, 1e+05]/No Screening")



# pathway probabilities ---------------------------------------------------

# calculate probabilitites along each branch, from root to leaf
path_probs <- treeSimR::calc_pathway_probs(osNode.cost)
osNode.cost$Set(path_probs = path_probs)

terminal_states <- data.frame(pathname = osNode.cost$Get('pathString', filterFun = isLeaf),
                              path_probs = osNode.cost$Get('path_probs', filterFun = isLeaf))
terminal_states

# collect final decision tree states in to
# competing risks model starting state groups

startstate.nonLTBI <- grepl("/Complete Treatment", x = terminal_states$pathname) | grepl("nonLTBI", x = terminal_states$pathname)
startstate.LTBI <- !startstate.nonLTBI

healthstatus <- NA
healthstatus[startstate.nonLTBI] <- "nonLTBI"
healthstatus[startstate.LTBI] <- "LTBI"

# proportion of individuals in LTBI or non-LTBI states after screening pathway
# setNames(object = aggregate(terminal_states$path_probs, by=list(healthstatus), FUN=sum), nm = c("state", "prob"))

# sample the subpopulation sizes at each terminal node and
# aggregate to competing risk model start states
start_state_proportions <- treeSimR::get_start_state_proportions(path_probs = terminal_states$path_probs,
                                                                 startstateid = healthstatus,
                                                                 samplesize = entryCohort_poptotal$pop[entryCohort_poptotal$year==year_cohort],
                                                                 numsamples = 2)


# expected values ---------------------------------------------------------

N.mc <- 10

# for defined nodes, sample of expected cost realisations
mc.cost <- treeSimR::MonteCarlo_expectedValues(osNode.cost, n = N.mc)

# for defined nodes, sample of expected health realisations
mc.health <- treeSimR::MonteCarlo_expectedValues(osNode.health, n = N.mc)


