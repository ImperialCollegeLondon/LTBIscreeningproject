#
# project: LTBI screening
# N Green
# Oct 2016
#
# simulation specific constants



# computation -------------------------------------------------------------

# number of Monte Carlo iterations
N.mc <- 10

# n.scenarios <- nrow(scenario_parameter_cost)
n.scenarios <- 10



# global constants --------------------------------------------------------

screen_age_range <- 18:35
# screen_age_range <- 18:45   ##TODO##

# year_cohort <- '2012' #latest full year
year_cohort <- '2009' #largest cohort


threshold <- 20000  #£


# create output folder

diroutput <- sprintf("ext-data/%d_to_%d_in_%s", min(screen_age_range), max(screen_age_range), year_cohort)
dir.create(diroutput)

