#
# project: LTBI screening
# N Green
# Oct 2016
#
# simulation specific constants


# computation -------------------------------------------------------------

# number of Monte Carlo iterations
N.mc <- 2

cluster <-  FALSE


# global constants --------------------------------------------------------

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

parameter_values_file <- system.file("data", "scenario-parameter-values_range-limits_with-LTBI-Tx-costs.xlsx",
                                     package = "LTBIscreeningproject")

# # create permanent output folder
# diroutput <- sprintf("ext-data/%d_to_%d_in_%s", min(screen_age_range), max(screen_age_range), year_cohort)
# dir.create(diroutput)

# create temporary output folder
diroutput <- tempdir()

plots_folder <- system.file("output", "plots",
                            package = "LTBIscreeningproject")
