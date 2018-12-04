
#' bcea_to_plotdata
#'
#' to use in mash, contour plotting functions
#'
#' @param bcea BCEA package object
#' @param folders list
#' @param wtp_threshold default: 20000
#'
#' @return
#' @export
#'
#' @examples
bcea_to_plotdata <- function(bcea,
                             folders,
                             wtp_threshold = "20000") {

  design_mat <-
    pastef(folders$output$parent,
           "scenario_params_df.csv") %>%
    read.csv() %>%
    design_matrix()

  out <-
    data.frame(
    INMB = bcea_incr$eib[bcea_incr$k == wtp_threshold, ],
    design_mat)

  return(out)
}
