#' scenario_QALYloss
#'
#' Splits output also into due to
#' morbidity and mortality.
#'
#' @param prop_avoided
#' @param endpoint
#' @param cohort
#'
#' @return
#' @export
#'
#' @examples
#'
scenario_QALYloss <- function(prop_avoided,
                              endpoint,
                              cohort) {

  assert_that(endpoint %in% c("death", "exit uk"))

  keep_tb <-
    switch(endpoint,
           "death" = cohort$all_tb,
           "exit uk" = cohort$uk_tb)

  fatality <- cohort$tb_fatality[keep_tb]
  QALY_statusquo <- cohort$QALY_statusquo[keep_tb]
  QALY_diseasefree <- cohort$QALY_diseasefree[keep_tb]
  id_avoided_tb <- cohort$id_avoided_tb[keep_tb]

  QALY_screened <- QALY_statusquo

  who_avoid <- rows_first_n_ids(id_avoided_tb,
                                prop_avoided)

  QALY_screened[who_avoid] <- QALY_diseasefree[who_avoid]

  QALYloss_statusquo <- QALY_diseasefree - QALY_statusquo
  QALYloss_screened  <- QALY_diseasefree - QALY_screened

  QALY_loss_statusquo_mortality <- sum(QALYloss_statusquo[fatality])
  QALY_loss_statusquo_morbidity <- sum(QALYloss_statusquo[!fatality])

  QALY_loss_statusquo_mort_pp <- QALY_loss_statusquo_mortality/sum(fatality)
  QALY_loss_statusquo_morb_pp <- QALY_loss_statusquo_morbidity/sum(!fatality)

  QALY_loss_screened_mortality <- sum(QALYloss_screened[fatality])
  QALY_loss_screened_morbidity <- sum(QALYloss_screened[!fatality])

  QALY_loss_screened_mort_pp <- QALY_loss_screened_mortality/sum(fatality)
  QALY_loss_screened_morb_pp <- QALY_loss_screened_morbidity/sum(!fatality)

  return(
    list(statusquo_mortality = QALY_loss_statusquo_mortality,
         statusquo_morbidity = QALY_loss_statusquo_morbidity,
         screened_mortality = QALY_loss_screened_mortality,
         screened_morbidity = QALY_loss_screened_morbidity,
         statusquo_mort_pp = QALY_loss_statusquo_mort_pp,
         statusquo_morb_pp = QALY_loss_statusquo_morb_pp,
         screened_mort_pp = QALY_loss_screened_mort_pp,
         screened_morb_pp = QALY_loss_screened_morb_pp)
  )
}
