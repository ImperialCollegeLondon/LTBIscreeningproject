
#' Assign LTBI status from country of origin
#'
#' @param IMPUTED_sample
#' @param pLatentTB.who
#'
#' @return
#' @export
#'
#' @examples
#'
assign_LTBI_status <- function(IMPUTED_sample,
                               pLatentTB.who) {

  who_levels <- c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]")
  who_level_breaks <- c(0, 50, 150, 250, 350, 100000)

  # match active TB prevalence groups in dataset to Pareek (2011)
  IMPUTED_sample$who_prev_cat_Pareek2011 <- cut(IMPUTED_sample$who_prevalence,
                                                breaks = who_level_breaks)

  IMPUTED_sample$who_prev_cat_Aldridge2016 <- cut(IMPUTED_sample$who_prevalence,
                                                  breaks = c(0, 39, 149, 349, 100000))

  data("TB_burden_countries", package = "LTBIscreeningproject")

  IMPUTED_sample <-
    merge(x = IMPUTED_sample,
          y = TB_burden_countries,
          by.x = c('iso_a3_country', 'year'),#issdt_year'),
          by.y = c('iso3', 'year'))


  IMPUTED_sample$who_inc_Pareek2011 <- cut(IMPUTED_sample$e_inc_100k,
                                           breaks = who_level_breaks)

  ### assume >35 == 35 year olds ###
  # i.e. age independent
  ##TODO: can be age-dependent

  pLatentTB.who_age <-
    matrix(data = pLatentTB.who,
           ncol = length(interv$screen_age_range),
           nrow = length(pLatentTB.who)) %>%
    data.frame(who_levels, .) %>%
    purrr::set_names("who_inc_Pareek2011",
                     as.character(interv$screen_age_range))

  # join with main data set
  pLatentTB.who_age.long <-
    pLatentTB.who_age %>%
    reshape2:::melt.data.frame(id.vars = "who_inc_Pareek2011",
                               value.name = "pLTBI",
                               variable.name = "age_at_screen")

  IMPUTED_sample <- merge(x = IMPUTED_sample,
                          y = pLatentTB.who_age.long,
                          by = c("age_at_screen",
                                 "who_inc_Pareek2011"))

  return(IMPUTED_sample)
}
