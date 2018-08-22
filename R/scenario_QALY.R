
#' Calculate total QALYs of a scenario
#'
#' @param prop_avoided probability
#' @param endpoint 'death' or 'exit uk'
#' @param cohort Individual data
#' @param ordered Should individuals have a fixed order when avoiding tb; default: TRUE
#'
#' @return list of status-quo and screened life-time QALYs
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


#
sample_avoid_lg <- function(id_avoided_tb,
                            prop_avoided,
                            ordered) {

  if (ordered) {
    res <-
      rows_first_n_ids(id_avoided_tb,
                       prop_avoided)
  }else{
    num_tb <- length(id_avoided_tb)
    avoid_id <-
      sample(seq_along(id_avoided_tb),
             size = prop_avoided*num_tb)
    res <- rep(FALSE, num_tb)
    res[avoid_id] <- TRUE
  }

  return(res)
}
