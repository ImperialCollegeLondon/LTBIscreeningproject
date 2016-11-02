#
# project: LTBI screening
# N Green
# Oct 2016
#
# main high-level script


# increase available RAM to store data
memory.size(max = 8000)

# data("IMPUTED_sample")

# total data set
load(file = "T:\\STATA-model_incidence data_updated\\IMPUTED_IOM_ETS_WHO_merged_15_2_9.RData")
IMPUTED_sample <- IMPUTED_IOM_ETS_WHO_merged_15_2_9
rm(IMPUTED_IOM_ETS_WHO_merged_15_2_9)


# define entry cohort -----------------------------------------------------

source("scripts/01-define-entry-cohort.R")


# decision tree -----------------------------------------------------------

source("scripts/02-decision-tree-model.R")


# survival model ----------------------------------------------------------

source("scripts/03-competing-risk-model.R")


# output plots ------------------------------------------------------------

source("scripts/04a-output-plots.R")
source("scripts/04b-output-plots.R")

