#
# project: LTBI screening
# N Green
# Oct 2016
#
# simulation specific constants


# computation -------------------------------------------------------------

# number of Monte Carlo iterations
N.mc <- 200

cluster <- TRUE

study <-  "QFT" #"twoway" #"oneway" "TSPOT" "HALT"

# LIFETIME_RISK <- 1
LIFETIME_RISK <- 0.163  #Choudhury (2013) 15 years
# LIFETIME_RISK <- 0.18   #2006 NICE economic appraisal
# LIFETIME_RISK <- 0.067  #Marks (2000) 40 years


# global constants --------------------------------------------------------

# time horizon for active TB progression
FUP_MAX_YEAR <- 100 #10, 20, 50

# screen_age_range <- 18:35
screen_age_range <- 18:45

# year_cohort <- '2012' #most recent complete year
year_cohort <- '2009' #largest cohort

# include QALYs and costs for individuals once they've left (i.e. to death)?
ENDPOINT_QALY <- "death" #"exit uk"
ENDPOINT_cost <- "exit uk" #"death"

# rather than screen _everyone_ on entry
# screen at random 0-5 years from entry
screen_0_to_5_year <- TRUE

# which incidence groups to screen
incidence_grps_screen <- c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]") #full set
# incidence_grps_screen <- c("(250,350]", "(350,1e+05]")


# folder locations --------------------------------------------------------

parameter_values_file <- system.file("data", sprintf("scenario-parameter-values_%s.xlsx", study),
                                     package = "LTBIscreeningproject")

# # create permanent output folder
diroutput <- sprintf("ext-data/%d_to_%d_in_%s_using_%s", min(screen_age_range), max(screen_age_range), year_cohort, study)
dir.create(diroutput)

# create temporary output folder
# diroutput <- tempdir()

plots_folder <- system.file("output", "plots",
                            package = "LTBIscreeningproject")

cluster_output_filename <- sprintf("decisiontree-results-%s.rds", study)

