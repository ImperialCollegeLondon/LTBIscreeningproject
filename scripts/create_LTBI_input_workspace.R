# ********************************************
# LTBI screening
# N Green
# 2017
#
# generate a fixed synthetic cohort and
# set constants; only need to run this once


data("051206 - IMPUTED_sample")


source("scripts/data-prep_policies.R")

source("scripts/01a-data-prep_interv-constants.R", echo = TRUE)

source("scripts/01b-data-prep_cost-effectiveness.R", echo = TRUE)

source("scripts/01-data-prep_scenario.R", echo = TRUE)

source("scripts/01c-data-prep_modelling.R", echo = TRUE)

source("scripts/04a_1-active-TB-extrapolation.R", echo = TRUE)

source("scripts/04a_3-include-new-tb-events.R", echo = TRUE)
