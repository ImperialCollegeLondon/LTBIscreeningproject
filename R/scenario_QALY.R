
#' Calculate total QALYs of a scenario
#'
#' @param prop_avoided probability cured of LTBI by screening
#' @param endpoint 'death' or 'exit uk' for time horizon
#' @param cohort Individual level data
#' @param ordered Should individuals have a fixed order when avoiding TB
#' This is useful for reproducability and ensures that a higher proportion avoiding TB is always better; default: TRUE
#'
#' @return list of status-quo and screened total QALYs
#' @export
#'
#' @examples
#'
scenario_QALY <- function(prop_avoided,
                          endpoint,
                          cohort,
                          ordered = TRUE) {

  assert_that(endpoint %in% c("death", "exit uk"))

  keep_tb <-
    switch(endpoint,
           "death" = cohort$all_tb,
           "exit uk" = cohort$uk_tb)

  QALY_statusquo <- cohort$QALY_statusquo[keep_tb]
  QALY_diseasefree <- cohort$QALY_diseasefree[keep_tb]
  id_avoided_tb <- cohort$id_avoided_tb[keep_tb]

  QALY_screened <- QALY_statusquo

  who_avoid <- sample_avoid_lg(id_avoided_tb,
                               prop_avoided,
                               ordered)

  QALY_screened[who_avoid] <- QALY_diseasefree[who_avoid]

  return(list(statusquo = sum(QALY_statusquo),
              screened = sum(QALY_screened)))
}

