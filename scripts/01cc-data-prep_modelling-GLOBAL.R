#**************************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# subset complete data set
# calculate associated stats


cohohot <- policy_cohort(cohort, interv)


# discount cost and QALYs in decision tree  ---------------------------------
## due to delayed start

prop_screen_year <- ceiling(cohort$screen_year) %>% prop_table()
screen_discount  <- prop_screen_year %*% QALY::discount(t_limit = length(prop_screen_year)) %>% c()

# year cohort size potentially screened
pop_year <- nrow(cohort)

# count numbers of tb cases -----------------------------------------------

n.exit_tb <- sum(cohort$exituk_tb)
n.uk_tb <- sum(cohort$uk_tb)
n.all_tb <- n.uk_tb + n.exit_tb

