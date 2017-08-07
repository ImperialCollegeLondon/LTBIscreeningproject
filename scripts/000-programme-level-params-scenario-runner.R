#
# LTBI screening
# N Green
#
# high-level global scenario runner
# for a determinstic sensitivity analysis
# of screening programme


devtools::load_all(".")

data("051206 - IMPUTED_sample")


# 1) define and clean data ------------------------------------------------

source("scripts/01a-data-prep_simulation-constants.R", echo = TRUE)
source("scripts/01b-data-prep_cost-effectiveness.R", echo = TRUE)
source("scripts/01c-data-prep_modelling.R", echo = TRUE)


# 4) event times estimation -----------------------------------------------

source("scripts/04a_1-active-TB-extrapolation.R")
source("scripts/04a_2-active-TB-imputation.R")
source("scripts/04bb-include-new-tb-events.R")

save.image(file = "data-ext/LTBI_input_workspace.RData")


#  ------------------------------------------------------------------------

data("global-parameters-scenarios")
data("global-parameters-scenarios_ls")

home_dir <- find.package("LTBIscreeningproject")

sources_correctly <- NULL

for (global_run in 2) {

  print(sprintf("[ programme level parameters ] scenario: %d", global_run))

  try_out <- try(source("scripts/00-main.R"))

  if (inherits(try_out, "try-error")) {
    setwd(home_dir)
  }

  sources_correctly <- c(sources_correctly, !inherits(try_out, "try-error"))
}

