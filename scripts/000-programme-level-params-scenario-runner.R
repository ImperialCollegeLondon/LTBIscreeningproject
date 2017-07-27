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

for (global_run in 1:length(global_params_scenarios_ls)) {

  print(sprintf("[ programme level parameters ] scenario: %d", global_run))
  source("scripts/00-main.R")
}
