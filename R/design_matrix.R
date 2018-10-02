
#' design_matrix
#'
#' Create a flat array from scenario inputs.
#'
#' @param params
#'
#' @return dataframe
#' @export
#'
#' @examples
#'   scenario_params_df %>%
#'    design_matrix()
#'
design_matrix <- function(params) {

  if (!"p" %in% names(params)) params$p <- NA_integer_

  params %>%
    transmute(scenario,
              name = paste(node, val_type, sep = "_"),
              val = ifelse(val_type == "cost", min, p)) %>%
    cast(scenario ~ name, value = 'val') %>%
    setNames(map_chr(names(.),
                     function(x) gsub(x = x, pattern = " ", replacement = "_")))
}
