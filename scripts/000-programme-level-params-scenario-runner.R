#
# LTBI screening
# N Green
#
# high-level global scenario runner
# for a determinstic sensitivity analysis
# of screening programme


devtools::load_all(".")

home_dir <- find.package("LTBIscreeningproject")

data("global-parameters-scenarios")
data("global-parameters-scenarios_ls")

sources_correctly <- NULL

for (global_run in 1:length(global_params_scenarios_ls)) {

  print(sprintf("[ programme level parameters ] scenario: %d", global_run))

  try_out <- try(source("scripts/00-main.R"))

  if (inherits(try_out, "try-error")) {
    setwd(home_dir)
  }

  sources_correctly <- c(sources_correctly, !inherits(try_out, "try-error"))
}
