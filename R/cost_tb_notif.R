
#' Total costs of first and secondary active TB cases
#'
#' @param num_sec_inf Number of secondary infections for each index TB case; vector of integer (0 or 1)
#' @param unit_cost Single value
#' @param notif_discounts for each index TB case; vector
#' @param secondary_inf_discounts for each index TB case; vector
#'
#' @return
#' @export
#'
#' @examples
#'
cost_tb_notif <- function(num_sec_inf,
                          unit_cost,
                          notif_discounts,
                          secondary_inf_discounts) {

  cost_secondary_inf <- num_sec_inf * unit_cost * secondary_inf_discounts
  total_cost <- (notif_discounts * unit_cost) + cost_secondary_inf
  return(total_cost)
}
