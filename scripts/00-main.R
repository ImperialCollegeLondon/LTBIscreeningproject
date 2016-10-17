#
# project: LTBI screening
# N Green
# Oct 2016
#
# main high-level script


data("IMPUTED_sample")


# define entry cohort -----------------------------------------------------

source("01-define-entry-cohort")


# decision tree -----------------------------------------------------------

source("02-decision-tree-model.R")


# survival model ----------------------------------------------------------

source("03-competing-risk-model.R")


# output plots ------------------------------------------------------------




