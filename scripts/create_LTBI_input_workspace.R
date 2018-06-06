# ********************************************
# LTBI screening
# N Green
# 2017
#
# generate a fixed cohort and set constants
# only need to run this once


data("051206 - IMPUTED_sample")

# 1) define and clean data ---------------------------------------------

source("scripts/global-parameters-environment-create.R")

source("scripts/01a-data-prep_interv-constants.R", echo = TRUE)

source("scripts/01b-data-prep_cost-effectiveness.R", echo = TRUE)

source("scripts/01-data-prep_scenario.R", echo = TRUE)

source("scripts/01c-data-prep_modelling.R", echo = TRUE)


# 4) event times estimation --------------------------------------------

source("scripts/04a_1-active-TB-extrapolation.R", echo = TRUE)

source("scripts/04a_3-include-new-tb-events.R", echo = TRUE)
