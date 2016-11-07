#
# project: LTBI screening
# N Green
# Oct 2016
#
# calculate cost-effectiveness active TB values


##TODO##
#
# need to calculate for the status quo too!
# so use uk_tb (original)
# then can add this to output plot code...
#
#
#
##


# fixed param values ------------------------------------------------------

# 12 month case fatality rate
cfr_age_lookup <- data.frame(age = c("[15,45)", "[45,65)", "[65,200)"),
                             cfr = c(0.0018, 0.0476, 0.1755),
                             a = c(1, 125, 413), #beta distns
                             b = c(564, 2500, 1940))

rownames(cfr_age_lookup) <- c("[15,45)", "[45,65)", "[65,200)")

QALYloss_TB_death <- 19.96

##TODO##
# could use expected death to calculate each QALY loss
# difference between time of death from active TB and all-cause time of death
# adjusted for utility of active TB

lifetime_QALYsloss <- 0.054

# treatment:
aTB_Tx_cost <- 5329
# gamma(8.333, 639.435)

# adverse effects?
# test: cost?

# create matching age bins in data
cfr_age <- cut(IMPUTED_sample$age_at_entry,
               breaks = c(15, 45, 65, 200),
               right = FALSE)



# total expected QALY losses and costs --------------------------------------------

# initiate output arrays
aTB_QALYloss <- array(data = NA,
                      dimnames = c("sim", "scenario"),
                      dim = c(n.uk_tbX, N.scenarios))
aTB_cost <- aTB_QALYloss

# all deterministic decision tree scenarios
for (scenario in seq_len(N.scenarios)){

  # within scenario samples
  for (i in uk_tbX_names){

    whos_aTB <- uk_tb_scenarios[ , i, scenario]==1

    # expected QALY loss due to active TB in total cohort
    E.lifetime_QALYsloss_cohort <- mean(lifetime_QALYsloss * whos_aTB)

    # expected QALY loss due to early death due to active TB
    E.QALYloss_death_aTB <- cfr_age_lookup[cfr_age, "cfr"] * QALYloss_TB_death

    # expected QALY loss due to early death over total cohort
    E.QALYloss_death_cohort <- mean(E.QALYloss_death_aTB * whos_aTB)

    # expected treatment cost over total cohort
    E.aTB_Tx_cost_cohort <- mean(aTB_Tx_cost * whos_aTB)

    aTB_QALYloss[i, scenario] <- E.lifetime_QALYsloss_cohort + E.QALYloss_death_cohort
    aTB_cost[i, scenario] <- E.aTB_Tx_cost_cohort
  }
}
