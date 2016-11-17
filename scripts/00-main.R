#
# project: LTBI screening
# N Green
# Oct 2016
#
# main high-level script


# increase RAM to store data
memory.size(max = 8000)

# data("IMPUTED_sample")

# total data set
load(file = "T:\\STATA-model_incidence data_updated\\IMPUTED_IOM_ETS_WHO_merged_15_2_9.RData")
IMPUTED_sample <- IMPUTED_IOM_ETS_WHO_merged_15_2_9
rm(IMPUTED_IOM_ETS_WHO_merged_15_2_9)


# define and clean data ---------------------------------------------------

source("scripts/01a-data-prep_simulation-constants.R")
source("scripts/01b-data-prep_modelling.R")
source("scripts/01c-data-prep_cost-effectiveness.R")


# decision tree -----------------------------------------------------------

source("scripts/02-decision-tree-model.R")


# survival models ---------------------------------------------------------

# source("scripts/03a-competing-risk-model_statusquo.R")
# source("scripts/03b-competing-risk-model_screening.R")


# cost-effectiveness ------------------------------------------------------

source("scripts/04a-cost-effectiveness_activeTB-samples.R")
source("scripts/04b-cost-effectiveness_QALY-costs.R")


# output plots ------------------------------------------------------------

# source("scripts/05a-output-plots_competing_risks.R")
source("scripts/05b-output-plots_cost_effectiveness.R")

