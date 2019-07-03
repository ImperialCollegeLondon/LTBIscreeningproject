
#' interv_constructor
#'
#' @param N.mc Global fixed constant; default 100
#' @param use_discount Global fixed constant
#' @param no_students TRUE/FALSE
#' @param force_everyone_stays
#' @param screen_with_delay Rather than screen _everyone_ on entry screen at random 0-5 years from entry
#' @param MAX_SCREEN_DELAY
#' @param FUP_MAX_YEAR Time horizon for active TB progression
#' @param screen_age_range
#' @param year_cohort year_cohort = '2012' is most recent complete year; largest cohort, corresponds with Pareek () LTBI risk
#' @param incidence_grps_screen Modified in the deterministic sensitivity analysis but set default values
#' @param min_screen_length_of_stay Modified in the deterministic sensitivity analysis but set default values
#' @param ENDPOINT_cost Modified in the deterministic sensitivity analysis but set default values
#' @param ENDPOINT_QALY Modified in the deterministic sensitivity analysis but set default values
#' @param LTBI_test
#'
#' @return
#' @export
#'
interv_constructor <- function(N.mc = 10000,
                               cluster = FALSE,
                               use_discount = TRUE,
                               no_students = FALSE,
                               force_everyone_stays = FALSE,
                               screen_with_delay = TRUE,
                               MAX_SCREEN_DELAY = 5,
                               FUP_MAX_YEAR = 100,
                               screen_age_range = 18:35,
                               # screen_age_range = 18:45,
                               year_cohort = '2009',
                               LTBI_test = "TSPOT",
                               # LIFETIME_RISK = 0.10
                               #"exit uk"
                               incidence_grps_screen = c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]"),
                               min_screen_length_of_stay = 0,
                               ENDPOINT_cost = "death",
                               ENDPOINT_QALY = "death") {

  ##TODO: add asserts

  interv <-
    list(force_everyone_stays = force_everyone_stays,
         # ENDPOINT_QALY = ENDPOINT_QALY,
         # ENDPOINT_cost = ENDPOINT_cost,
         # incidence_grps_screen = incidence_grps_screen,
         # min_screen_length_of_stay = min_screen_length_of_stay,
         MAX_SCREEN_DELAY = MAX_SCREEN_DELAY,
         screen_with_delay = screen_with_delay,
         FUP_MAX_YEAR = FUP_MAX_YEAR,
         screen_age_range = screen_age_range,
         year_cohort = year_cohort,
         N.mc = N.mc,
         cluster = cluster,
         no_students = no_students,
         LTBI_test = LTBI_test,
         discount_rate = dplyr::if_else(use_discount, 0.035, 0))

  save(interv, file = here::here("data", "intervention_constants.RData"))

  invisible(interv)
}
