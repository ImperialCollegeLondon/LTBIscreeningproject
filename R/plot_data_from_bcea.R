
#' inmb_from_bcea
#'
#' @param bcea
#' @param folders
#' @param wtp_threshold
#'
#' @return list by wtp
#' @export
#'
#' @examples
#'
inmb_from_bcea <- function(bcea,
                           folders,
                           wtp_threshold = c(10000, 20000, 30000)) {

  if (!all(is.na(folders))) {
    design_mat <-
      pastef(folders$output$parent,
             "scenario_params_df.csv") %>%
      read.csv() %>%
      design_matrix()
  }

  n_comparisons <- bcea$n.comparisons

  ##TODO: refactor this loop
  eib <- NULL

  for (i in seq_along(wtp_threshold)) {

    eib <- c(eib, round(bcea$eib[bcea$k %in% wtp_threshold, ][i, ], 2))
  }

  eib_mat <- data.frame(scenario = seq_len(n_comparisons),
                        wtp = rep(wtp_threshold, each = n_comparisons),
                        INMB = eib)

  inmb_mat <-
    merge(x = design_mat,
          y = eib_mat,
          by = "scenario") %>%
    arrange(scenario, wtp)

  if (!all(is.na(folders))) {
    save(inmb_mat,
         file = pastef(folders$output$scenario, "inmb_mat.RData"))
  }

  # return as list
  inmb_mat <- split(inmb_mat, inmb_mat$wtp)

  return(inmb_mat)
}
