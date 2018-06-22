#**************************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# set-up folder locations

# setup_folders()
# setup_folders <- function(policy,
#                           interv,
#                           policy_ls,
#                           parent_folder) {

policy_name <- policies_ls[policy]

# create permanent output folder
parent_folder <- sprintf("ext-data/%d_to_%d_in_%s",
                         min(interv$screen_age_range),
                         max(interv$screen_age_range),
                         interv$year_cohort)

diroutput <- sprintf("%s/%s", parent_folder, policy_name)
dir.create(parent_folder, showWarnings = FALSE)
dir.create(diroutput, showWarnings = FALSE)


plots_folder <- system.file("output", "plots",
                            package = "LTBIscreeningproject")
plots_folder_scenario <- sprintf("%s/%s", plots_folder, policy_name)
dir.create(plots_folder_scenario, showWarnings = FALSE)

# save --------------------------------------------------------------------

file.copy(from = "data/scenario_parameters_df.csv",
          to = pastef(parent_folder, "scenario_parameters_df.csv"))

file.copy(from = "data/policies-inputs.csv",
          to = pastef(parent_folder, "policies-inputs.csv"))

#}
