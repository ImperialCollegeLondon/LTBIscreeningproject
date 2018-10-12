
#' ---
#' title: "LTBI screening model:
#' simulation specific constants"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---


# policy-level sensitivity parameters ----------------------------------

policy_name <- policies_ls[policy]

if (exists("policy")) {

  interv <- policy_interv(policy_name,
                          interv)
}

# subset complete data set ----------------------------------------------
# so can over-write

# if (!exists("cohort")) {

  cohort <- policy_cohort(IMPUTED_sample,
                          policy_name,
                          interv)
# }

screen_discount <- screen_discount(cohort)

# size potentially screened
pop_year <- nrow(cohort)
