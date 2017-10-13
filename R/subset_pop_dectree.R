
# absolute numbers

# num_screen_year <- table(ceiling(IMPUTED_sample_year_cohort$screen_year))
# map(num_screen_year,
#     function(x) subset_pop_dectree(dtr_clone, n_pop = x))

# subset_abs_dectree <- function(dtr_clone,
#                                n_pop) {
#
#   dtr_clone$Set(E_n = round(dtr_clone$Get('path_probs') * n_pop, 2))
#
#   tab <- my_ToDataFrameTypeCol(dtr_clone, "path_probs", "p", "E_n")
#
#   num_LTBI_pre <- dplyr::filter(tab, level_3 == "LTBI" & is.na(level_4))
#   num_tests <- dplyr::filter(tab, level_4 == "Agree to Screen", is.na(level_5))
#   num_positive <-
#     dplyr::filter(tab,
#                   (level_3 == "LTBI" & level_5 == "Sensitivity" & is.na(level_6)) |
#                     (level_3 == "non-LTBI" & level_5 == "1-Specificity" & is.na(level_6)))
#   num_startTx <- dplyr::filter(tab, level_6 == "Start Treatment", is.na(level_7))
#   num_completeTx <- dplyr::filter(tab, level_9 == "Complete Treatment", is.na(level_10))
#   num_cured <- dplyr::filter(tab, level_10 == "Effective")
#
#   data.frame(num_LTBI_pre = sum(num_LTBI_pre$E_n),
#              num_tests = sum(num_tests$E_n),
#              num_positive = sum(num_positive$E_n),
#              num_startTx = sum(num_startTx$E_n),
#              num_completeTx = sum(num_completeTx$E_n),
#              num_cured = sum(num_cured$E_n),
#              num_LTBI_post = sum(num_LTBI_pre$E_n) - sum(num_cured$E_n))
# }


#' subset_pop_dectree
#'
#' @param osNode
#'
#' @return
#' @export
#'
#' @examples
subset_pop_dectree <- function(osNode) {

  dectree_df <- my_ToDataFrameTypeCol(osNode, "path_probs", "p")

  LTBI_pre <- dplyr::filter(dectree_df,
                            level_3 == "LTBI", is.na(level_4))
  tests <- dplyr::filter(dectree_df,
                         level_4 == "Agree to Screen", is.na(level_5))
  positive <- dplyr::filter(dectree_df,
                            (level_3 == "LTBI" & level_5 == "Sensitivity" & is.na(level_6)) |
                            (level_3 == "non-LTBI" & level_5 == "1-Specificity" & is.na(level_6)))
  startTx <- dplyr::filter(dectree_df,
                           level_6 == "Start Treatment", is.na(level_7))
  completeTx <- dplyr::filter(dectree_df,
                              level_9 == "Complete Treatment", is.na(level_10))
  cured <- dplyr::filter(dectree_df,
                             level_10 == "Effective")

  data.frame(LTBI_pre = sum(LTBI_pre$path_probs),
             tests = sum(tests$path_probs),
             positive = sum(positive$path_probs),
             startTx = sum(startTx$path_probs),
             completeTx = sum(completeTx$path_probs),
             cured = sum(cured$path_probs),
             LTBI_post = sum(LTBI_pre$path_probs) - sum(cured$path_probs))
}
