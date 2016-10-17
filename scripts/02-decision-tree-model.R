#
# project: LTBI screening
# N Green
# Oct 2016
#
# screening pathway decision tree model


library(data.tree)
library(treeSimR)


# define decision tree
osNode <- treeSimR::costeffectiveness_tree(yaml_tree = "data/LTBI_dtree-cost-entry2012.yaml")
print(osNode, "type", "p", "distn", "mean", "sd")

osNode <- treeSimR::calc_expectedValues(osNode)
print(osNode, "type", "p", "distn", "mean", "sd", "payoff")

treeSimR::MonteCarlo_expectedValues(osNode, n=100)

# calculate probabilitites alond each branch, from root to leaf
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

#
aggregate(terminal_states$path_probs, by=list(healthstatus), FUN=sum)

#
treeSimR::get_start_state_proportions(terminal_states$path_probs, healthstatus, samplesize, numsamples)

##TODO##

# calculate expected cost

# calculate expected health detriment




