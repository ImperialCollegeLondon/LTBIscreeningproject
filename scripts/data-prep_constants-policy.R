# **************************************
# project: LTBI screening
# N Green
# Oct 2016
# data-prep_constants-policy.R
#
# simulation specific constants


# policy-level sensitivity parameters -------------------------------------------

if (exists("policy")) {

  get_current_policy <- get_from_envir(policies_ls[policy])

  interv$incidence_grps_screen <- get_current_policy("incidence_grps_screen")
  interv$min_screen_length_of_stay <- get_current_policy("min_screen_length_of_stay")
  interv$ENDPOINT_cost <- get_current_policy("ENDPOINT_cost")
  interv$ENDPOINT_QALY <- get_current_policy("ENDPOINT_QALY")
  interv$LTBI_test <- get_current_policy("LTBI_test")
  interv$treatment <- get_current_policy("treatment")
}

message(sprintf("[ policy level parameters ]\n policy: %s\n WHO groups: %s\n min stay: %s\n cost endpoint: %s\n QALY endpoint: %s\n test: %s\n treatment: %s",
                green(policy),
                green(paste(interv$incidence_grps_screen, collapse = "")),
                green(interv$min_screen_length_of_stay),
                green(interv$ENDPOINT_cost),
                green(interv$ENDPOINT_QALY),
                green(interv$LTBI_test),
                green(interv$treatment)))


# subset complete data set ----------------------------------------------

# so can over-write
if (!exists("cohort")) {

  cohort <- policy_cohort(IMPUTED_sample, interv)
}

screen_discount <- screen_discount(cohort)

# size potentially screened
pop_year <- nrow(cohort)
