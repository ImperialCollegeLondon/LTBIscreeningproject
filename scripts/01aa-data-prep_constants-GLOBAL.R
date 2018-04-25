#**************************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# simulation specific constants


# policy-level sensitivity parameters -------------------------------------------

if (exists("policy")) {

  get_current_policy <- get_from_envir(policies_ls[policy])

  interv$incidence_grps_screen <- get_current_policy("incidence_grps_screen")
  interv$min_screen_length_of_stay <- get_current_policy("min_screen_length_of_stay")
  interv$ENDPOINT_cost <- get_current_policy("ENDPOINT_cost")
  interv$ENDPOINT_QALY <- get_current_policy("ENDPOINT_QALY")
  interv$LTBI_test <- get_current_policy("LTBI_test")
  interv$treatment <- get_current_policy("treatment")
}

message(sprintf("[ policy level parameters ]\n policy: %s\n WHO groups: %s\n min stay: %s\n cost endpoint: %s\n QALY endpoint: %s\n test: %s\n treatment: %s",
                green(policy),
                green(paste(interv$incidence_grps_screen, collapse = "")),
                green(interv$min_screen_length_of_stay),
                green(interv$ENDPOINT_cost),
                green(interv$ENDPOINT_QALY),
                green(interv$LTBI_test),
                green(interv$treatment)))


# folder locations --------------------------------------------------------

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

