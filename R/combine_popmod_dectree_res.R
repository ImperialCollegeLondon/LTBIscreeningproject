
#' combine_popmod_dectree_res
#'
#' Combine cost and QALY outputs from decision tree model and population model
#' for overall cost-effectiveness samples.
#'
#' @param cohort individual level data
#' @param interv fixed model run inputs
#' @param popmod_res output of \code{activetb_qaly_cost()}
#' @param dectree_res output of \code{parallel_decision_tree()}
#' @param folders list of ouput folder locations
#'
#' @return list of cost-effective statistics:
#' \itemize{
#'   \item ce0: marginal status-quo. Costs and QALYs of each sim.
#'   \item ce1: marginal intervention. Costs and QALYs of each sim.
#'   \item ce_default: non-incremental cost-effectiveness i.e. dataframe with first column status-quo.
#'   \item ce_incr: incremental cost-effectivness i.e. dataframe with first column 0 and other screening cost minus status-quo.
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

  ce_res <- list(
    ce0 = ce0,
    ce1 = ce1,
    ce_default = ce_default,
    ce_incr = ce_incr)

  save(ce_res, file = pastef(folders$output$scenario, "ce_res.RData"))

  return(ce_res)
}


