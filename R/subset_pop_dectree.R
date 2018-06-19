
#' Subset Populations of Decision Tree
#'
#' Specific to the LTBI screening model,
#' this gives the total probabilities of
#' particular state on the pathway by summing
#' across nodes, using pathprobs.
#'
#' TODO: can we use data.tree:: functions instead
#' TODO: convert to data.table?
#'
#' @param osNode data.tree object
#'
#' @return data.frame of probabilities
#' @export
#'
#' @examples
#'
subset_pop_dectree <- function(osNode) {

  dectree_df <- my_ToDataFrameTypeCol(osNode, "path_probs", "p")


  osNode$Set(node_names = osNode$Get('name'))


  ##TODO: why does this only return first match?
  # osNodeClone <- Clone(osNode, pruneFun = function(x) x$node_names == 'LTBI')

  ##TODO: speed this up by not using dectree_df...

  LTBI_pre <- osNode$Get('path_probs', filterFun = function(x) x$node_names == 'LTBI')
  # LTBI_pre      <- leaf_df_by_name(osNode, node_name = "LTBI")$path_probs

  # tests         <- leaf_df_by_name(osNode, node_name = "Agree to Screen")$path_probs
  tests <- osNode$Get('path_probs', filterFun = function(x) x$node_names == "Agree to Screen")


  LTBI_tests    <- dplyr::filter(dectree_df,
                                 (level_3 == "LTBI" & level_4 == "Agree to Screen" & is.na(level_5)))
  positive      <- dplyr::filter(dectree_df,
                                 (level_3 == "LTBI" & level_5 == "Sensitivity" & is.na(level_6)) |
                                   (level_3 == "non-LTBI" & level_5 == "1-Specificity" & is.na(level_6)))
  LTBI_positive <- dplyr::filter(dectree_df,
                                 (level_3 == "LTBI" & level_5 == "Sensitivity" & is.na(level_6)))
  startTx       <- leaf_df_by_name(osNode, node_name = "Start Treatment")
  LTBI_startTx  <- dplyr::filter(dectree_df,
                                 (level_3 == "LTBI" & level_6 == "Start Treatment" & is.na(level_7)))
  completeTx    <- leaf_df_by_name(osNode, node_name = "Complete Treatment")
  LTBI_completeTx <- dplyr::filter(dectree_df,
                                   (level_3 == "LTBI" & level_9 == "Complete Treatment" & is.na(level_10)))
  cured         <- dplyr::filter(dectree_df,
                                 level_10 == "Effective")

  data.frame(LTBI_pre = sum(LTBI_pre),
             tests = sum(tests),
             positive = sum(positive$path_probs),
             startTx = sum(startTx$path_probs),
             completeTx = sum(completeTx$path_probs),
             cured = sum(cured$path_probs),
             LTBI_post = sum(LTBI_pre) - sum(cured$path_probs)) %>%
    mutate(p_LTBI_to_cured = cured/LTBI_pre,
           LTBI_tests = sum(LTBI_tests$path_probs)/LTBI_pre,
           LTBI_positive = sum(LTBI_positive$path_probs)/LTBI_pre,
           LTBI_startTx = sum(LTBI_startTx$path_probs)/LTBI_pre,
           LTBI_completeTx = sum(LTBI_completeTx$path_probs)/LTBI_pre)
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

  if (length(level) > 1) stop('more than one level')

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

