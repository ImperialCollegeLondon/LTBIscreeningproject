
#' sample_subset_pop_dectree
#'
#' @param osNode data.tree object
#' @param n Sample size
#' @param sample_p TRUE/FALSE
#'
#' @return matrix
#' @export
#'
#' @examples
#'
sample_subset_pop_dectree <- function(osNode,
                                      n = 1,
                                      sample_p = TRUE) {

  out1 <- subset_pop_dectree(osNode = osNode)
  out <-
    matrix(NA, nrow = n, ncol = ncol(out1)) %>%
    `colnames<-`(colnames(out1))

  for (i in seq_len(n)) {

    osNode$Set(path_probs =
                 calc_pathway_probs(osNode,
                                    sample_p = TRUE))
    out[i, ] <-
      subset_pop_dectree(osNode = osNode) %>%
      as.matrix()
  }

  out
}
