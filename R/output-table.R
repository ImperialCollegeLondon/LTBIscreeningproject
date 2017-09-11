
#' Table of Cost-Effectiveness Statistics
#'
#'
#' @param bcea_obj Pre-calculated output from BCEA package
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
table_costeffectiveness <- function(bcea_obj,
                                    wtp_threshold = 20000) {

  out <- with(bcea_obj,
              do.call(data.frame,
                      list("percentile_5th" = apply(ib[k == wtp_threshold, , ], 2, quantile, probs = 0.05),
                           "EINB" = eib[k == wtp_threshold, ],
                           "percentile_95th" = apply(ib[k == wtp_threshold, , ], 2, quantile, probs = 0.95),
                           "ceac_WTP15000" = ceac[k == 15000, ],
                           "ceac_WTP20000" = ceac[k == 20000, ],
                           "ceac_WTP25000" = ceac[k == 25000, ],
                           "ceac_WTP30000" = ceac[k == 30000, ]))
  )

  return(out)
}


#' table_tb_avoided
#'
#' @param mc_n.tb_screen
#'
#' @return
#' @export
#'
#' @examples
#'
table_tb_avoided <- function(mc_n.tb_screen) {

  out <- NULL
  QUANTILES <- c(0.05, 0.5, 0.95)

  for (i in seq_along(mc_n.tb_screen)) {

    diseasefree_all <- subset(mc_n.tb_screen[[i]]$n.tb_screen.all_tb,
                              status == "disease-free",
                              select = "n") %>% unlist()

    diseasefree_uk <- subset(mc_n.tb_screen[[i]]$n.tb_screen.uk_tb,
                             status == "disease-free",
                             select = "n") %>% unlist()

    out <- rbind(out, c(diseasefree_uk %>% quantile(probs = QUANTILES),
                        diseasefree_all %>% quantile(probs = QUANTILES)))

  }

  colnames(out) <- paste(c("EWNI","EWNI","EWNI",
                           "Total","Total","Total"), colnames(out))

  return(out)
}

