
#' combine_popmod_dectree_res
#'
#' @param cohort
#' @param interv
#' @param popmod_res
#' @param dectree_res
#' @param folders
#'
#' @return
#' \itemize{
#'   \item ce0: marginal status-quo
#'   \item ce1: marginal intervention
#'   \item ce_default: non_incremental cost-effectiveness
#'   \item ce_incr: incremental cost-effectivness
#'   }
#' @export
#'
#' @examples
#'
combine_popmod_dectree_res <- function(cohort,
                                       interv,
                                       popmod_res,
                                       dectree_res,
                                       folders = NA) {

  t_dectree <- list(cost_person = purrr::map(dectree_res, "mc_cost"),
                    QALY_person = purrr::map(dectree_res, "mc_health"))

  screen_discount <- screen_discount(cohort,
                                     interv$discount_rate)

  ce_incr <- make_incremental_ce(popmod_res,
                                 t_dectree,
                                 screen_discount,
                                 folders)

  ce0 <- make_ce0(popmod_res)

  ce1 <- make_ce1(popmod_res,
                  t_dectree,
                  screen_discount)

  ce_default <- ce_default(ce0, ce1)

  list(ce0 = ce0,
       ce1 = ce1,
       ce_default = ce_default,
       ce_incr = ce_incr)
}


