
#' Create policy cohort
#'
#' Filter individuals by policy definition.
#'
#' @param cohort_in total sample
#' @param policy_name
#' @param interv list of conditions
#'
#' @return cohort
#' @export
#'
policy_cohort <- function(cohort_in,
                          policy_name,
                          interv) {

  diroutput <- diroutput(policy_name, interv)

  # single year cohort only
  cohort <- dplyr::filter(cohort_in,
                          .data$issdt_year %in% interv$year_cohort)

  # uk stay long enough
  cohort <- dplyr::filter(cohort,
                          .data$date_exit_uk1_issdt.years >= interv$min_screen_length_of_stay)

  if (interv$screen_with_delay) {
    cohort <- dplyr::filter(cohort,
                            .data$screen == 1)}

  if (interv$no_students) {
    cohort <- dplyr::filter(cohort,
                            .data$visatype2 != "Students")}

  # remove individuals from 'lower' incidence countries
  cohort <- dplyr::filter(cohort,
                          .data$who_inc_Pareek2011 %in% interv$incidence_grps_screen)

  # assign each tb case unique id
  cohort <- set_id_avoided_tb(cohort)

  save(cohort, file = pastef(diroutput, "cohort.RData"))

  return(cohort)
}

#
set_id_avoided_tb <- function(cohort) {

  cohort$id_avoided_tb <- NA

  cohort$id_avoided_tb[cohort$all_tb] <-
    {set.seed(111); sample.int(sum(cohort$all_tb), replace = FALSE)}

  return(cohort)
}
