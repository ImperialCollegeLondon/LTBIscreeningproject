#
# project: LTBI screening
# N Green
# Oct 2016
#
# simulation specific constants


# computation -------------------------------------------------------------

# number of Monte Carlo iterations
N.mc <- 2000

cluster <- TRUE


# global variables --------------------------------------------------------

# or read in scenarios environments
if (exists("global_run")) {

  incidence_grps_screen <- get("incidence_grps_screen", envir = eval(parse(text = global_params_scenarios_ls[global_run])))
  min_screen_length_of_stay <- get("min_screen_length_of_stay", envir = eval(parse(text = global_params_scenarios_ls[global_run])))

}else{

  # select which paramter file for decision tree
  study <-  "QFT" #"twoway" #"oneway" "TSPOT" "HALT"

  # which incidence groups to screen
  incidence_grps_screen <- c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]") #full set
  # incidence_grps_screen <- c("(150,250]", "(250,350]", "(350,1e+05]")
  # incidence_grps_screen <- c("(250,350]", "(350,1e+05]")

  min_screen_length_of_stay <- 0 #years #5
}


# global constants --------------------------------------------------------

# rather than screen _everyone_ on entry
# screen at random 0-5 years from entry
screen_with_delay <- TRUE

MAX_SCREEN_DELAY <- 5

# time horizon for active TB progression
FUP_MAX_YEAR <- 100 #10, 20, 50

# screen_age_range <- 18:35
screen_age_range <- 18:45

# year_cohort <- '2012' #most recent complete year
year_cohort <- '2009' #largest cohort

# include QALYs and costs for individuals once they've left (i.e. to death)?
ENDPOINT_QALY <- "death" #"exit uk"
ENDPOINT_cost <- "exit uk" #"death"

# LIFETIME_RISK <- 1
LIFETIME_RISK <- 0.163  #Choudhury (2013) 15 years
# LIFETIME_RISK <- 0.18   #NICE economic appraisal (2006)
# LIFETIME_RISK <- 0.067  #Marks (2000) 40 years


# folder locations --------------------------------------------------------

# parameter_values_file <- system.file("data", sprintf("scenario-parameter-values_%s.xlsx", study),
#                                      package = "LTBIscreeningproject")

parameter_values_file <- system.file("data", "scenario-parameter-values.xlsx",
                                     package = "LTBIscreeningproject")

# # create permanent output folder
parent_folder <- sprintf("ext-data/%d_to_%d_in_%s", min(screen_age_range), max(screen_age_range), year_cohort)
diroutput <- sprintf("%s/%s", parent_folder, global_params_scenarios_ls[global_run])
dir.create(parent_folder)
dir.create(diroutput)

# create temporary output folder
# diroutput <- tempdir()

plots_folder <- system.file("output", "plots",
                            package = "LTBIscreeningproject")
plots_folder_scenario <- sprintf("%s/%s", plots_folder, global_params_scenarios_ls[global_run])
dir.create(plots_folder_scenario)

cluster_output_filename <- sprintf("decisiontree-results_%s.rds", format(Sys.time(), "%Y-%m-%d %I-%p"))

