#**************************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# subset complete data set
# calculate associated stats


cohort <- policy_cohort(cohort, interv)

screen_discount <- screen_discount(cohort)

# year cohort size potentially screened
pop_year <- nrow(cohort)

n.exit_tb <- sum(cohort$exituk_tb)
n.uk_tb <- sum(cohort$uk_tb)
n.all_tb <- n.uk_tb + n.exit_tb

