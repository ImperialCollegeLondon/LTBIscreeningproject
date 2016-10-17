#
# project: LTBI screening
# N Green
# Oct 2016
#
# screening and treatment pathway decision tree model


library(data.tree)
library(treeSimR)


# define decision tree
osNode <- treeSimR::costeffectiveness_tree(yaml_tree = "data/LTBI_dtree-cost-entry2012.yaml")
print(osNode, "type", "p", "distn", "mean", "sd")

# calculate the expected value for each node
osNode <- treeSimR::calc_expectedValues(osNode)
print(osNode, "type", "p", "distn", "mean", "sd", "payoff")

# for defined nodes, return sample of expected value realisations
treeSimR::MonteCarlo_expectedValues(osNode, n=100)

# calculate probabilitites along each branch, from root to leaf
path_probs <- treeSimR::calc_pathway_probs(osNode)
osNode$Set(path_probs = path_probs)

terminal_states <- data.frame(pathname = osNode$Get('pathString', filterFun = isLeaf),
                              path_probs = osNode$Get('path_probs', filterFun = isLeaf))
terminal_states

# group final decision tree states in to competing risks model starting state groups
startstate.nonLTBI <- grepl("/Complete Treatment", x = terminal_states$pathname) | grepl("nonLTBI", x = terminal_states$pathname)
startstate.LTBI <- !startstate.nonLTBI

healthstatus <- NA
healthstatus[startstate.nonLTBI] <- "nonLTBI"
healthstatus[startstate.LTBI] <- "LTBI"

# proportion of individuals in LTBI or non-LTBI states after screening pathway
setNames(object = aggregate(terminal_states$path_probs, by=list(healthstatus), FUN=sum), nm = c("state", "prob"))

# sample the subpopulation sizes at each terminal node and
# aggregate to competing risk model start states
treeSimR::get_start_state_proportions(path_probs = terminal_states$path_probs,
                                      startstateid = healthstatus,
                                      samplesize = entryCohort_poptotal$pop[entryCohort_poptotal$year=="2012"],
                                      numsamples = 2)


##TODO##

# calculate expected cost

# calculate expected health detriment




