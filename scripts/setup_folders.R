#**************************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# set-up folder locations


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

cluster_output_filename <- sprintf("decisiontree-results_%s_%s.rds", policy_name,
                                   format(Sys.time(), "%Y-%m-%d %I-%p"))

save(interv, file = pastef(diroutput, "interv.RData"))
