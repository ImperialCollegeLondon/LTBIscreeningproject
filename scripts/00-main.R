#
# project: LTBI screening
# N Green
# Oct 2016
#
# Main top-level script


# packages
devtools::load_all(".")

load("../../data/051206 - IMPUTED_sample.RData")


# 1) define and clean data ------------------------------------------------

source("scripts/01a-data-prep_simulation-constants.R", echo = TRUE)
source("scripts/01b-data-prep_cost-effectiveness.R", echo = TRUE)
source("scripts/01c-data-prep_modelling.R", echo = TRUE)
source("scripts/01d-data-prep_competing-risks.R")


# 2) decision tree --------------------------------------------------------

source("scripts/02-decision-tree-model.R")


# 4) cost-effectiveness ---------------------------------------------------

source("scripts/04a_1-active-TB-extrapolation.R")
source("scripts/04a_2-active-TB-imputation.R")
source("scripts/04b_1-include-new-tb-events.R")

source("scripts/04b_2-activeTB-postscreen-samples.R")
source("scripts/04c-cost-effectiveness_QALY-costs.R")


# 5) output plots/tables --------------------------------------------------

# source("scripts/05a-output-plots_competing-risks.R")
source("scripts/05b-output-plots_cost-effectiveness.R")
source("scripts/05e-output-plots_cost-effectiveness_active-TB-cases.R") ##TODO:
source("scripts/05f-tornado_plots.R")
source("scripts/05g-histograms.R")
# source("scripts/05h-CE_summary_stats.R")


# clean-up session --------------------------------------------------------
if (grepl("temp", diroutput, ignore.case = TRUE))
  unlink(diroutput)
