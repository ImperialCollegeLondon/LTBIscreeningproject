
#' Impute LTBI Status From Country of Origin
#'
#' @param data
#' @param pLatentTB.who
#'
#' @return
#' @export
#'
#' @examples
#'
impute_LTBI_status <- function(data, pLatentTB.who) {

  # prevalance group frequencies in data
  tab.who <- table(data$who_prev_cat_Pareek2011)
  tab.who_uktb <- table(data$who_prev_cat_Pareek2011, data$uk_tb)

  # adjusted probability of LTBI for non-active TB given observed active TB cases
  # p(LTBI|not active TB) = (N * p(LTBI) - #(active TB))/#(not active TB)
  pLatentTB.who_adjusted <- (tab.who * pLatentTB.who - tab.who_uktb[ ,"1"])/tab.who_uktb[ ,"0"]

  # probability of LTBI for each non-active TB case
  prob <- with(data, pLatentTB.who_adjusted[who_prev_cat_Pareek2011])

  # sample LTBI status
  data$LTBI <- (runif(n = length(prob)) < prob)

  # if active TB then assumed LTBI
  data$LTBI[data$uk_tb=="1"] <- TRUE

  return(data)
}

