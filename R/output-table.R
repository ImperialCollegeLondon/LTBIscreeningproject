#' table_costeffectiveness
#'
#' @param bcea_obj
#'
#' @return
#' @export
#'
#' @examples
table_costeffectiveness <- function(bcea_obj,
                                    wtp_threshold) {

  attach(bcea_obj)

  out <- do.call(data.frame,
                 list("percentile_5th" = apply(ib[k == wtp_threshold, , ], 2, quantile, probs = 0.05),
                      "EINB" = eib[k == wtp_threshold, ],
                      "percentile_95th" = apply(ib[k == wtp_threshold, , ], 2, quantile, probs = 0.95),
                      "ceac_WTP15000" = ceac[k == 15000, ],
                      "ceac_WTP20000" = ceac[k == 20000, ],
                      "ceac_WTP25000" = ceac[k == 25000, ],
                      "ceac_WTP30000" = ceac[k == 30000, ]))
  detach(bcea_obj)

  return(out)
}

