#**************************************************************************
# project: LTBI screening
# N Green
# Oct 2016
#
# simulation specific constants


# global fixed constants --------------------------------------------------

force_everyone_stays <- FALSE

ENDPOINT_QALY <- "death" #"exit uk"

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

# LIFETIME_RISK <- 0.10
# LIFETIME_RISK <- 0.163  #Choudhury (2013) 15 years
# LIFETIME_RISK <- 0.18   #NICE economic appraisal (2006)
# LIFETIME_RISK <- 0.067  #Marks (2000) 40 years


# deterministic sensitivity analysis
# grid of input parameter values

# scenario_file_tag <- "_high-low"
# scenario_file_tag <- "_main" #paste0("_", study) #_oneway
# scenario_file_tag <- "_fullfactorial_QFT-GIT"
scenario_file_tag <- "_fullfactorial_QFT-GIT_3mo_RIFINH"
# scenario_file_tag <- "_fullfactorial_QFT-GIT_50testcost"
# scenario_file_tag <- "_fullfactorial_QFT-GIT_100testcost"
# scenario_file_tag <- "_fullfactorial_QFT-plus"
# scenario_file_tag <- "_fullfactorial_TSPOT"
