
#' Subset Populations of Decision Tree
#'
#' Specific to the LTBI screening model, this gives the total
#' probabilities of particular state on the pathway by summing
#' across nodes, using pathprobs.
#'
#' TODO: absolute counts also
#' TODO: can we use data.tree:: functions instead
#' TODO: convert to data.table?
#'
#' @param osNode data.tree object
#'
#' @return data.frame of probabilities
#' @export
#'
#' @examples
subset_pop_dectree <- function(osNode) {

  dectree_df <- my_ToDataFrameTypeCol(osNode, "path_probs", "p")

  LTBI_pre <- leaf_df_by_name(osNode, node_name = "LTBI")

  tests <- leaf_df_by_name(osNode, node_name = "Agree to Screen")

  positive <- dplyr::filter(dectree_df,
                            (level_3 == "LTBI" & level_5 == "Sensitivity" & is.na(level_6)) |
                            (level_3 == "non-LTBI" & level_5 == "1-Specificity" & is.na(level_6)))

  startTx <- leaf_df_by_name(osNode, node_name = "Start Treatment")

  completeTx <- leaf_df_by_name(osNode, node_name = "Complete Treatment")

  cured <- dplyr::filter(dectree_df,
                         level_10 == "Effective")

  data.frame(LTBI_pre = sum(LTBI_pre$path_probs),
             tests = sum(tests$path_probs),
             positive = sum(positive$path_probs),
             startTx = sum(startTx$path_probs),
             completeTx = sum(completeTx$path_probs),
             cured = sum(cured$path_probs),
             LTBI_post = sum(LTBI_pre$path_probs) - sum(cured$path_probs)) %>%
    mutate(p_LTBI_to_cured = cured/LTBI_pre)
}


#' leaf_df_by_name
#'
#' Subset to dataframe of terminal nodes by name.
#'
#' @param dectree_df my_ToDataFrameTypeCol() output
#' @param node_name Text string
#'
#' @return dataframe of subset
#' @export
#'
#' @examples
#'
leaf_df_by_name <- function(osNode,
                            node_name) {

  dectree_df <- my_ToDataFrameTypeCol(osNode, "path_probs", "p")

  level <-
    osNode$Get('level',
               filterFun = function(x) x$name == node_name) %>%
    unique()

  if (length(level) > 1) stop('Error')

  ##TODO: how to do NSE with variable names as strings?
  # dplyr::filter(dectree_df,
  #               paste0("level_", level) == node_name,
  #               is.na(paste0("level_", level + 1)))

  which_rows <-
    dectree_df[ ,paste0("level_", level)] == node_name &
    !is.na(dectree_df[ ,paste0("level_", level)] == node_name) &
    is.na(dectree_df[ ,paste0("level_", level + 1)])

  return(dectree_df[which_rows, ])
}

