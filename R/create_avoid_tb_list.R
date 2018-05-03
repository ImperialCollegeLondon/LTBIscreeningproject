
#' create_avoid_tb_list
#'
#' Numbers of individuals who avoid getting TB due to screening
#'
#' @param scenario_res
#' @param n_all_tb
#' @param n_uk_tb
#'
#' @return list
#' @export
#'
#' @examples
#'
create_avoid_tb_list <- function(scenario_res,
                                 n_all_tb,
                                 n_uk_tb) {

  avoid_all_tb <-
    scenario_res$subset_pop %>%
    map(as.tibble) %>%
    map(~select(.x, "p_LTBI_to_cured")) %>%
    map(`*`, n_all_tb) %>%
    map(`names<-`, 'death')

  avoid_uk_tb <-
    scenario_res$subset_pop %>%
    map(as.tibble) %>%
    map(~select(.x, "p_LTBI_to_cured")) %>%
    map(`*`, n_uk_tb) %>%
    map(`names<-`, 'exit uk')

    mapply(function(x,y) cbind(x, y),
           avoid_all_tb,
           avoid_uk_tb,
           SIMPLIFY = FALSE)
}
