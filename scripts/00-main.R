#
# project: LTBI screening
# N Green
# Oct 2016
#
# Main top-level script


# packages
devtools::load_all(".")

suppressWarnings(warning("library"))

# increase RAM to store data
memory.size(max = 8000)

load("../../data/051206 - IMPUTED_sample.RData")

# total data set
# load(file = "T:\\STATA-model_incidence data_updated\\IMPUTED_IOM_ETS_WHO_merged_15_2_9.RData")
# IMPUTED_sample <- IMPUTED_IOM_ETS_WHO_merged_15_2_9
# rm(IMPUTED_IOM_ETS_WHO_merged_15_2_9)


# 1) define and clean data ------------------------------------------------

source("scripts/01a-data-prep_simulation-constants.R", echo = TRUE)
source("scripts/01b-data-prep_cost-effectiveness.R", echo = TRUE)
source("scripts/01c-data-prep_modelling.R", echo = TRUE)


# 2) decision tree --------------------------------------------------------

source("scripts/02-decision-tree-model.R")


# 3) survival models ------------------------------------------------------

source("scripts/03a-competing-risks.R")


# 4) cost-effectiveness ---------------------------------------------------

source("scripts/04a-cost-effectiveness_activeTB-samples.R")
source("scripts/04aa-active-TB-imputation_extrapolation.R")
source("scripts/04b-cost-effectiveness_QALY-costs.R")


# 5) output plots/tables --------------------------------------------------

# source("scripts/05a-output-plots_competing-risks.R")
source("scripts/05b-output-plots_cost-effectiveness.R")
source("scripts/05e-output-plots_cost-effectiveness_active-TB-cases.R")
source("scripts/05f-tornado_plots.R")
source("scripts/05g-histograms.R")
source("scripts/05h-CE_summary_stats.R")


# clean-up session --------------------------------------------------------
if(grepl("temp", diroutput, ignore.case = TRUE))
  unlink(diroutput)
