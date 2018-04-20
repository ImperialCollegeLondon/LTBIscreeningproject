
#' Total Costs of First and Secondary Active TB Cases
#'
#' @param num_sec_inf
#' @param unit_cost
#' @param notif_discounts
#' @param secondary_inf_discounts
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
