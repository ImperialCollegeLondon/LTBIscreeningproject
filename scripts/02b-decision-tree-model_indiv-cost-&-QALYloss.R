#
# project: LTBI screening
# N Green
# Feb 2017
#
# individual cost incurred and QALY loss using decision tree model


# sample tree cost incurred per individual -------------------------------

clone_LTBI.cost <- Clone(osNode.cost$`(50,150]`$LTBI)
class(clone_LTBI.cost) <- c("costeffectiveness_tree", class(clone_LTBI.cost))
clone_LTBI.cost$Set(type = "logical", filterFun = function(x) x$level==1)

clone_nonLTBI.cost <- Clone(osNode.cost$`(50,150]`$'non-LTBI')
class(clone_nonLTBI.cost) <- c("costeffectiveness_tree", class(clone_nonLTBI.cost))
clone_nonLTBI.cost$Set(type = "logical", filterFun = function(x) x$level==1)


IMPUTED_sample_year_cohort$screen_cost[IMPUTED_sample_year_cohort$LTBI ==
                                         1] <-
  treeSimR::MonteCarlo_expectedValues(osNode = clone_LTBI.cost,
                                      n = sum(IMPUTED_sample_year_cohort$LTBI))$`expected values`

IMPUTED_sample_year_cohort$screen_cost[IMPUTED_sample_year_cohort$LTBI ==
                                         0] <-
  treeSimR::MonteCarlo_expectedValues(osNode = clone_nonLTBI.cost,
                                      n = sum(1 -
                                                IMPUTED_sample_year_cohort$LTBI))$`expected values`

# plots

LTBI_cost <- IMPUTED_sample_year_cohort$screen_cost[IMPUTED_sample_year_cohort$LTBI==1]
nonLTBI_cost <- IMPUTED_sample_year_cohort$screen_cost[IMPUTED_sample_year_cohort$LTBI==0]

par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
hist(LTBI_cost, breaks = 100, main = "")
hist(nonLTBI_cost, breaks = 100, main = "")
mtext("Histograms of screening costs", outer = TRUE, cex = 1.5)

par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
plot(density(LTBI_cost, adjust=2), main = "")
plot(density(nonLTBI_cost, adjust=2), col = "red", main = "")
mtext("Densities of screening costs", outer = TRUE, cex = 1.5)



# sample tree QALY loss per individual -------------------------------

clone_LTBI.health <- Clone(osNode.health$`(50,150]`$LTBI)
class(clone_LTBI.health) <- c("costeffectiveness_tree", class(clone_LTBI.health))
clone_LTBI.health$Set(type = "logical", filterFun = function(x) x$level==1)

clone_nonLTBI.health <- Clone(osNode.health$`(50,150]`$'non-LTBI')
class(clone_nonLTBI.health) <- c("costeffectiveness_tree", class(clone_nonLTBI.health))
clone_nonLTBI.health$Set(type = "logical", filterFun = function(x) x$level==1)


IMPUTED_sample_year_cohort$screen_health[IMPUTED_sample_year_cohort$LTBI ==
                                           1] <-
  treeSimR::MonteCarlo_expectedValues(osNode = clone_LTBI.health,
                                      n = sum(IMPUTED_sample_year_cohort$LTBI))$`expected values`

IMPUTED_sample_year_cohort$screen_health[IMPUTED_sample_year_cohort$LTBI ==
                                           0] <-
  treeSimR::MonteCarlo_expectedValues(osNode = clone_nonLTBI.health,
                                      n = sum(1 -
                                                IMPUTED_sample_year_cohort$LTBI))$`expected values`

# plots

LTBI_QALYloss <- IMPUTED_sample_year_cohort$screen_health[IMPUTED_sample_year_cohort$LTBI==1]
nonLTBI_QALYloss <- IMPUTED_sample_year_cohort$screen_health[IMPUTED_sample_year_cohort$LTBI==0]

par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
hist(LTBI_QALYloss, breaks = 100, main = "")
hist(nonLTBI_QALYloss, breaks = 100, main = "")
mtext("Histograms of screening QALY loss", outer = TRUE, cex = 1.5)

par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
plot(density(LTBI_QALYloss, adjust=2), main = "")
plot(density(nonLTBI_QALYloss, adjust=2), col = "red", main = "")
mtext("Densities of screening QALY loss", outer = TRUE, cex = 1.5)

