# ******************************************
#
# project: LTBI screening
# N Green
# Oct 2016
#
# subset complete data set
# calculate associated stats


# so can over-ride
if (!exists("cohort")) {

  cohort <- policy_cohort(IMPUTED_sample, interv)
}

screen_discount <- screen_discount(cohort)

# year cohort size potentially screened
pop_year <- nrow(cohort)
