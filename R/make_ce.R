
#' make_ce0
#'
#' @param popmod_res
#'
#' @return
#' @export
#'
#' @examples
#'
make_ce0 <- function(popmod_res) {

  popmod_qaly <- list_to_BCEA(popmod_res$QALY.statusquo_person)
  popmod_cost <- list_to_BCEA(popmod_res$cost.statusquo_person)

  list(e = as.matrix(popmod_qaly),
       c = as.matrix(popmod_cost))
}


#' make_ce1
#'
#' @param popmod_res
#' @param t_dectree
#' @param sdiscount
#'
#' @return
#' @export
#'
#' @examples
#'
make_ce1 <- function(popmod_res,
                     t_dectree,
                     sdiscount) {

  LTBI_qaly <- list_to_BCEA(t_dectree$QALY_person, -sdiscount)
  LTBI_cost <- list_to_BCEA(t_dectree$cost_person, sdiscount)

  popmod_qaly <- list_to_BCEA(popmod_res$QALY.screened_person)
  popmod_cost <- list_to_BCEA(popmod_res$cost.screened_person)

  list(e = as.matrix(LTBI_qaly + popmod_qaly),
       c = as.matrix(LTBI_cost + popmod_cost))
}


#' ce_default
#'
#' Uses the first column of the status-quo matrices
#' for all status-quo comparisons.
#'
#' @param ce0
#' @param ce1
#'
#' @return
#' @export
#'
#' @examples
#'
ce_default <- function(ce0,
                       ce1) {

  scenario_names <- c(0, seq_len(ncol(ce0$e)))

  e_default <-
    data.frame(ce0$e[ ,1], ce1$e) %>%
    set_names(scenario_names)

  c_default <-
    data.frame(ce0$c[ ,1], ce1$c) %>%
    set_names(scenario_names)

  list(e = as.matrix(e_default),
       c = as.matrix(c_default))
}
