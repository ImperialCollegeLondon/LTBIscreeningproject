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

# print(osNode.cost, "type", "p", "distn", "mean", "sd", limit = NULL)


# assign cohort WHO group branching proportions, for given year
who_levels <- names(entryCohort_who_prop[[year_cohort]])

for (i in seq_along(who_levels)){

  osNode.cost$Set(p = entryCohort_who_prop[[year_cohort]][i],
                  filterFun = function(x) x$name==who_levels[i])

  osNode.health$Set(p = entryCohort_who_prop[[year_cohort]][i],
                    filterFun = function(x) x$name==who_levels[i])
}

# only used for probability calc
# cost and health == 0
osNode.noscreen <- Clone(osNode.cost)


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
# osNode.health$Set(p = 0, filterFun = function(x) x$name=="Screening")
# osNode.health$Set(p = 1, filterFun = function(x) x$name=="No Screening")
# osNode.health$Set(p = 1, filterFun = function(x) x$pathString=="LTBI screening cost/(350, 1e+05]/Screening")
# osNode.health$Set(p = 0, filterFun = function(x) x$pathString=="LTBI screening cost/(350, 1e+05]/No Screening")


# pathway probabilities ---------------------------------------------------

# calculate total probabilitites along each branch, from root to leaf
## screening
path_probs.screen <- treeSimR::calc_pathway_probs(osNode.cost)
osNode.cost$Set(path_probs = path_probs.screen)
terminal_states.screen <- data.frame(pathname = osNode.cost$Get('pathString', filterFun = isLeaf),
                                     path_probs = osNode.cost$Get('path_probs', filterFun = isLeaf))

## no screening
path_probs.noscreen <- treeSimR::calc_pathway_probs(osNode.noscreen)
osNode.noscreen$Set(path_probs = path_probs.noscreen)
terminal_states.noscreen <- data.frame(pathname = osNode.noscreen$Get('pathString', filterFun = isLeaf),
                                       path_probs = osNode.noscreen$Get('path_probs', filterFun = isLeaf))

# collect final decision tree states in to
# competing risks model starting state groups

startstate.nonLTBI <- grepl("/Complete Treatment", x = terminal_states.screen$pathname) | grepl("non-LTBI", x = terminal_states.screen$pathname)
startstate.LTBI <- !startstate.nonLTBI

terminal_states.noscreen$healthstatus <- terminal_states.screen$healthstatus <- NA
terminal_states.noscreen$healthstatus[startstate.nonLTBI] <- terminal_states.screen$healthstatus[startstate.nonLTBI] <- "non-LTBI"
terminal_states.noscreen$healthstatus[startstate.LTBI] <- terminal_states.screen$healthstatus[startstate.LTBI] <- "LTBI"

# sample the subpopulation sizes at each terminal node and
# aggregate to competing risk model start states

n.startstate <- 2

start_state_proportions.screen <- treeSimR::get_start_state_proportions(path_probs = terminal_states.screen$path_probs,
                                                                        startstateid = terminal_states.screen$healthstatus,
                                                                        samplesize = entryCohort_poptotal$pop[entryCohort_poptotal$year==year_cohort],
                                                                        numsamples = n.startstate)

start_state_proportions.noscreen <- treeSimR::get_start_state_proportions(path_probs = terminal_states.noscreen$path_probs,
                                                                          startstateid = terminal_states$healthstatus,
                                                                          samplesize = entryCohort_poptotal$pop[entryCohort_poptotal$year==year_cohort],
                                                                          numsamples = n.startstate)

start_state_proportions.noscreen.mean <- setNames(aggregate(x = terminal_states.noscreen$path_probs,
                                                            by = list(terminal_states.noscreen$healthstatus), FUN = sum), nm = c("state", "prob"))

start_state_proportions.screen.mean <- setNames(aggregate(x = terminal_states.screen$path_probs,
                                                          by = list(terminal_states.screen$healthstatus), FUN = sum), nm = c("state", "prob"))


# probability successfully complete treatment of LTBI
p.LTBI_to_nonLTBI <- round(osNode.cost$Get('path_probs', filterFun = function(x) x$name=="Complete Treatment")/
                           osNode.cost$Get('path_probs', filterFun = function(x) x$name=="LTBI"),
                           digits = 4)
p.LTBI_to_nonLTBI <- unique(p.LTBI_to_nonLTBI[!p.LTBI_to_nonLTBI%in%c(NA,NaN,Inf)])


# expected values ---------------------------------------------------------

N.mc <- 10

# for defined nodes, sample expected cost
mc.cost <- treeSimR::MonteCarlo_expectedValues(osNode.cost, n = N.mc)

# for defined nodes, sample expected health
mc.health <- treeSimR::MonteCarlo_expectedValues(osNode.health, n = N.mc)


