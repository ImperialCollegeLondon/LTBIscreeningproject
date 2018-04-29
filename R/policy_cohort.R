

#' policy_cohort
#'
#' @param cohort
#' @param interv
#'
#' @return
#' @export
#'
#' @examples
#'
policy_cohort <- function(cohort,
                          interv) {

  # single year cohort only
  cohort <- dplyr::filter(IMPUTED_sample,
                          issdt_year == interv$year_cohort)

  # uk stay long enough
  cohort <- dplyr::filter(cohort,
                          date_exit_uk1_issdt.years >= interv$min_screen_length_of_stay)

  if (interv$screen_with_delay) {
    cohort <- dplyr::filter(cohort,
                            screen == 1)}

  if (interv$no_students) {
    cohort <- dplyr::filter(cohort,
                            visatype2 != "Students")}

  # remove individuals from 'lower' incidence countries
  cohort <- dplyr::filter(cohort,
                          who_prev_cat_Pareek2011 %in% interv$incidence_grps_screen)

  cohort$id_avoided_tb[cohort$all_tb] <- {set.seed(111); sample.int(sum(cohort$all_tb), replace = FALSE)}


  save(cohort, file = "data/cohort.RData")
  save(cohort, file = pastef(diroutput, "cohort.RData"))

  return(cohort)
}