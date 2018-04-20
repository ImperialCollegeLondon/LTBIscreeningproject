#**************************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# simulation specific constants


# policy-level sensitivity parameters -------------------------------------------

# or read in scenarios environments
if (exists("global_run")) {

  get_current_scenario <- get_from_envir(global_params_scenarios_ls[global_run])

  interv$incidence_grps_screen <- get_current_scenario("incidence_grps_screen")
  interv$min_screen_length_of_stay <- get_current_scenario("min_screen_length_of_stay")
  interv$ENDPOINT_cost <- get_current_scenario("ENDPOINT_cost")
  interv$LTBI_test <- get_current_scenario("LTBI_test")
  interv$treatment <- get_current_scenario("treatment")
}

message(sprintf("[ policy level parameters ]\n WHO groups: %s\n min stay: %s\n cost endpoint: %s\n test: %s\n treatment: %s",
                green(paste(interv$incidence_grps_screen, collapse = "")),
                green(interv$min_screen_length_of_stay),
                green(interv$ENDPOINT_cost),
                green(interv$LTBI_test),
                green(interv$treatment)))


# folder locations --------------------------------------------------------

scenario_name <- global_params_scenarios_ls[global_run]

# create permanent output folder
parent_folder <- sprintf("ext-data/%d_to_%d_in_%s",
                         min(interv$screen_age_range),
                         max(interv$screen_age_range),
                         interv$year_cohort)

diroutput <- sprintf("%s/%s", parent_folder, scenario_name)
dir.create(parent_folder, showWarnings = FALSE)
dir.create(diroutput, showWarnings = FALSE)


plots_folder <- system.file("output", "plots",
                            package = "LTBIscreeningproject")
plots_folder_scenario <- sprintf("%s/%s", plots_folder, scenario_name)
dir.create(plots_folder_scenario, showWarnings = FALSE)

cluster_output_filename <- sprintf("decisiontree-results_%s_%s.rds", scenario_name,
                                   format(Sys.time(), "%Y-%m-%d %I-%p"))

save(interv, file = pastef(diroutput, "interv.RData"))

