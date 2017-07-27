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

for (global_run in 7:length(ls_global_params_scenarios)) {

  print(sprintf("global runner: %d", global_run))
  source("scripts/00-main.R")
}
