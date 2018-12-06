
#' interv_constructor
#'
#' @param N.mc
#' @param use_discount
#' @param no_students
#' @param force_everyone_stays
#' @param screen_with_delay
#' @param MAX_SCREEN_DELAY
#' @param FUP_MAX_YEAR
#' @param screen_age_range
#' @param year_cohort
#' @param incidence_grps_screen
#' @param min_screen_length_of_stay
#' @param ENDPOINT_cost
#' @param ENDPOINT_QALY
#'
#' @return
#' @export
#'
interv_constructor <- function(N.mc = 1, #100
                               cluster = FALSE,

                               # global fixed constants
                               use_discount = TRUE,
                               no_students = FALSE,
                               force_everyone_stays = FALSE,

                               # rather than screen _everyone_ on entry
                               # screen at random 0-5 years from entry
                               screen_with_delay = TRUE,
                               MAX_SCREEN_DELAY = 5,

                               # time horizon for active TB progression
                               FUP_MAX_YEAR = 100,
                               screen_age_range = 18:35,
                               # screen_age_range = 18:45

                               # year_cohort = '2012' #most recent complete year
                               #largest cohort, corresponds with Pareek () LTBI risk
                               year_cohort = '2009',

                               # LIFETIME_RISK = 0.10

                               # these parameters will be modified in the
                               # deterministic sensitivity analysis
                               # but set default values
                               #"exit uk"
                               incidence_grps_screen = c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]"),
                               min_screen_length_of_stay = 0,
                               ENDPOINT_cost = "death",
                               ENDPOINT_QALY = "death") {

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
         no_students = no_students,
         discount_rate = if_else(use_discount, 0.035, 0))

  save(interv, file = here::here("data", "intervention_constants.RData"))

  invisible(interv)
}
