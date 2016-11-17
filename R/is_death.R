
#' Is Follow-up Time a Time of Death
#'
#' Create event-type indicators
#'
#' fup_limit <- 19723 is days from 1960-01-01
#' TODO is.exit_uk, is.fup_limit
#'
#' @param imputation_num
#' @param data
#' @param fup_limit
#'
#' @return
#' @export
#'
#' @examples
#'
is.death <- function(imputation_num, data,
                     fup_limit = 19723){

  date_deathX <- paste("date_death", imputation_num, sep="")
  date_exit_ukX <- paste("date_exit_uk", imputation_num, sep="")
  fupX <- paste("fup", imputation_num, sep="")

  return(data[ ,date_deathX] <= data[ ,date_exit_ukX] &
           data$uk_tb==0 &
           data[ ,fupX]!=fup_limit)
}

