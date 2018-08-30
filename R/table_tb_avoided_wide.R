
#' table_tb_avoided_wide
#'
#' @param dectree_res
#' @param folder text string
#'
#' @return EWNI and total 5%, 50% and 95% quantiles
#' @export
#'
#' @examples
#'
table_tb_avoided_wide <- function(dectree_res,
                                  folder = NA) {

  n_tb_screen_all <- map(dectree_res, "n_tb_screen_all")
  n_tb_screen_uk <- map(dectree_res, "n_tb_screen_uk")

  out <- NULL
  QUANTILES <- c(0.05, 0.5, 0.95)

  for (i in seq_along(n_tb_screen_all)) {

    diseasefree_all <- subset(n_tb_screen_all[[i]],
                              status == "disease-free",
                              select = "n") %>% unlist()

    diseasefree_uk <- subset(n_tb_screen_uk[[i]],
                             status == "disease-free",
                             select = "n") %>% unlist()

    out <- rbind(out, c(diseasefree_uk %>% quantile(probs = QUANTILES),
                        diseasefree_all %>% quantile(probs = QUANTILES)))

  }

  colnames(out) <- paste(c("EWNI","EWNI","EWNI",
                           "Total","Total","Total"), colnames(out))

  if (!is.na(folder)) {
    write.csv(x = out,
              file = paste(folder, "tb_avoided_table.csv", sep = "/"))
  }

  invisible(out)
}
