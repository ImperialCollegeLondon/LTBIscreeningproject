#
# LTBI screening
# N Green
#
# high-level global scenario runner
# for a determinstic sensitivity analysis
# of screening programme


devtools::load_all(".")

data("global-parameters-scenarios")
data("global-parameters-scenarios_ls")

sources_correctly <- NULL

for (global_run in 1:length(global_params_scenarios_ls)) {

  print(sprintf("[ programme level parameters ] scenario: %d", global_run))

  try_error <- try(source("scripts/00-main.R"))

  sources_correctly <- c(try_error, !inherits(sources_correctly, "try-error"))
}
