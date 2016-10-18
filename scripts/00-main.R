#
# project: LTBI screening
# N Green
# Oct 2016
#
# main high-level script


data("IMPUTED_sample")


# define entry cohort -----------------------------------------------------

source("scripts/01-define-entry-cohort.R")


# decision tree -----------------------------------------------------------

source("scripts/02-decision-tree-model.R")


# survival model ----------------------------------------------------------

source("scripts/03-competing-risk-model.R")


# output plots ------------------------------------------------------------

source("scripts/04-output-plots.R")


