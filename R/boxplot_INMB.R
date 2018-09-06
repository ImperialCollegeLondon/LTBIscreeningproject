
#' boxplot_INMB
#'
#' @param bcea
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
boxplot_INMB <- function(bcea, ...) {

  UseMethod("boxplot_INMB", bcea)
}


#' @rdname boxplot_INMB
#'
boxplot_INMB.bcea <- function(bcea,
                              folders = NA,
                              wtp_threshold = 20000) {

  ##TODO: oneway/twoway conditions

  if (!all(is.na(folders))) {

    design_mat <-
      pastef(folders$output$parent,
             "scenario_params_df.csv") %>%
      read.csv() %>%
      design_matrix()

    filename <- paste(folders$plots$scenario, "boxplot.png", sep = "/")
    png(filename, width = 400, height = 350, res = 45)
    on.exit(dev.off())
  }

  if (oneway) {

    dat <-
      bcea$ib[bcea$k == wtp_threshold, , ] %>%
      melt() %>%
      mutate(X2 = factor(X2))

    print(
      ggplot(dat, aes(x = X2, y = value, color = X2, group = X2)) +
        geom_boxplot())

  }else if (twoway) {

    other_variable <- names(design_mat)[!names(design_mat) %in% c("scenario", "Agree_to_Screen_cost")]

    dat2 <-
      t(bcea$ib[bcea$k == wtp_threshold, , ]) %>%
      cbind.data.frame(design_mat, .) %>%
      melt.data.frame(id.vars = names(design_mat)) %>%
      dplyr::rename(INB = value) %>%
      dplyr::mutate(Agree_to_Screen_cost = factor(Agree_to_Screen_cost))

    print(
      ggplot(dat2, aes_string(x = other_variable, y = "INB", group = "scenario", fill = "Agree_to_Screen_cost")) +
        geom_boxplot())
  }
}


# # # for start/ complete TX
# dat2 <-
#   t(bcea$ib[bcea$k == wtp_threshold, , ]) %>%
#   cbind.data.frame(design_mat, .) %>%
#   melt.data.frame(id.vars = names(design_mat)) %>%
#   dplyr::rename(INB = value) %>%
#   dplyr::mutate(Agree_to_Screen_cost = factor(Agree_to_Screen_cost)) %>%
#   dplyr::mutate(Start_Treatment_p = factor(Start_Treatment_p))
#
# print(
#   ggplot(dat2, aes_string(x = other_variable[1], y = "INB", group = "scenario", fill = other_variable[2])) +
#     geom_boxplot())
