#**************************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# simulation specific constants


N.mc <- 100

cluster <- FALSE


# global fixed constants --------------------------------------------------

no_students <- FALSE

force_everyone_stays <- FALSE


# rather than screen _everyone_ on entry
# screen at random 0-5 years from entry
screen_with_delay <- TRUE

MAX_SCREEN_DELAY <- 5

# time horizon for active TB progression
FUP_MAX_YEAR <- 100

screen_age_range <- 18:35
# screen_age_range <- 18:45

# year_cohort <- '2012' #most recent complete year
year_cohort <- '2009' #largest cohort, corresponds with Pareek () LTBI risk

# LIFETIME_RISK <- 0.10
# LIFETIME_RISK <- 0.163  #Choudhury (2013) 15 years
# LIFETIME_RISK <- 0.18   #NICE economic appraisal (2006)
# LIFETIME_RISK <- 0.067  #Marks (2000) 40 years

# these parameters will be modified in the
# deterministic sensitivity analysis
# but set default values
incidence_grps_screen <- c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]")
min_screen_length_of_stay <- 0
ENDPOINT_cost <- "death"
ENDPOINT_QALY <- "death" #"exit uk"


interv <-
  list(force_everyone_stays = force_everyone_stays,
       # ENDPOINT_QALY = ENDPOINT_QALY,
       # ENDPOINT_cost = ENDPOINT_cost,
       # incidence_grps_screen = incidence_grps_screen,
       # min_screen_length_of_stay = min_screen_length_of_stay,
       screen_with_delay = screen_with_delay,
       FUP_MAX_YEAR = FUP_MAX_YEAR,
       screen_age_range = screen_age_range,
       year_cohort = year_cohort,
       N.mc = N.mc,
       cluster = cluster,
       no_students = no_students)

save(interv,
     file = "data/intervention_constants.RData")

