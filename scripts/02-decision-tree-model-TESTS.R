#
# project: LTBI screening
# N Green
# Oct 2016
#
# LTBI screening and treatment pathway decision tree model
#
# SIMPLIFIED VALUES E.G. 0 OR 1 PROBABILITIES


library(readxl)
library(data.tree)
library(treeSimR)


# output to screen
options("max.print" = 3000)


# initiate decision tree --------------------------------------------------

osNode.cost.fileName <- system.file("data", "LTBI_dtree-cost_01-TEST.yaml", package = "LTBIscreeningproject")
osNode.health.fileName <- system.file("data", "LTBI_dtree-QALYloss_01-TEST.yaml", package = "LTBIscreeningproject")

n.scenarios <- 1


# create decision tree objects
## cost
costeff.cost <- treeSimR::costeffectiveness_tree(yaml_tree = osNode.cost.fileName)
osNode.cost <- costeff.cost$osNode


## health
costeff.health <- treeSimR::costeffectiveness_tree(yaml_tree = osNode.health.fileName)
osNode.health <- costeff.health$osNode

print(costeff.cost)
print(costeff.health)


# sensitivity analysis ----------------------------------------------------
# iterate over each deterministic scenario of parameter values

# delete old output files
if(file.exists(paste(diroutput, "mc_cost.csv", sep = "/"))) file.remove(paste(diroutput, "mc_cost.csv", sep = "/"))
if(file.exists(paste(diroutput, "mc_health.csv", sep = "/"))) file.remove(paste(diroutput, "mc_health.csv", sep = "/"))
if(file.exists(paste(diroutput, "prob_complete_Tx_given_LTBI_by_who.csv", sep = "/"))) file.remove(paste(diroutput, "prob_complete_Tx_given_LTBI_by_who.csv", sep = "/"))


# pathway probabilities ---------------------------------------------------

# calculate total probabilities along each branch, from root to leaf
path_probs.screen <- treeSimR::calc_pathway_probs(osNode.cost)
osNode.cost$Set(path_probs = path_probs.screen)

print(osNode.cost)

# total prob successfully cured of LTBI for each WHO category -------------

# number of ways to effectively compelete Tx per LTBI
LTBItreeClone <- Clone(osNode.cost$LTBI,
                       pruneFun = function(x) myPruneFun(x, "Effective"))

Effective.leafCount <- LTBItreeClone$leafCount

p.complete_Tx <- osNode.cost$Get('path_probs',
                                 filterFun = function(x) x$name=="Effective")

p.LTBI <- osNode.cost$Get('path_probs',
                          filterFun = function(x) x$name=="LTBI")


# sum path_probs over all leafs in WHO groups

Effective.groups <- rep(seq_along(p.LTBI), each = Effective.leafCount)

p.Effective <- aggregate(p.complete_Tx, by = list(Effective.groups), FUN = sum)$x

p.complete_Tx_given_LTBI_by_who <- p.Effective/p.LTBI


# sample total expected values --------------------------------------------

mc.cost <- treeSimR::MonteCarlo_expectedValues(osNode = osNode.cost, n = N.mc)
mc.health <- treeSimR::MonteCarlo_expectedValues(osNode.health, n = N.mc)



# save --------------------------------------------------------------------

# cost-effectiveness outputs
cat(x = paste(as.numeric(mc.cost$`expected values`), collapse = ","),
    file = paste(diroutput, "mc_cost.csv", sep = "/"), append = TRUE, fill = TRUE)

cat(x = paste(as.numeric(mc.health$`expected values`), collapse = ","),
    file = paste(diroutput, "mc_health.csv", sep = "/"), append = TRUE, fill = TRUE)

# defined pathway probabilities
cat(x = paste(as.numeric(p.complete_Tx_given_LTBI_by_who), collapse = ","),
    file = paste(diroutput, "prob_complete_Tx_given_LTBI_by_who.csv", sep = "/"), append = TRUE, fill = TRUE)

