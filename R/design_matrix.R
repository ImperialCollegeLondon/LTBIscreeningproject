
#' design_matrix
#'
#' @param params
#'
#' @return
#' @export
#'
#' @examples
#'   scenario_params_df %>%
#'    design_matrix()
#'
design_matrix <- function(params) {

  params %>%
    transmute(scenario,
              name = paste(node, val_type, sep = "_"),
              val = ifelse(val_type == "cost", min, p)) %>%
    cast(scenario ~ name, value = 'val') %>%
    setNames(map_chr(names(.),
                     function(x) gsub(x = x, pattern = " ", replacement = "_")))
}
