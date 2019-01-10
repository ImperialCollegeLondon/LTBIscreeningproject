
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
                              wtp_threshold = 20000,
                              oneway = FALSE) {

  ##TODO: oneway/twoway conditions

  if (!all(is.na(folders))) {

    design_mat <-
      pastef(folders$output$parent,
             "scenario_params_df.csv") %>%
      read.csv() %>%
      design_matrix()

    filename <- pastef(folders$plots$scenario, "boxplot.png")
  }

  if (oneway) {

    dat <-
      bcea$ib[bcea$k == wtp_threshold, , ] %>%
      melt() %>%
      merge(design_mat, by.x = "X2", by.y = "scenario") %>%
      mutate(X2 = factor(X2),
             Agree_to_Screen_cost = factor(Agree_to_Screen_cost))

    print(
      INMB_boxplot <-
        ggplot(dat, aes(x = Agree_to_Screen_cost, y = value, color = Pop_cost, group = X2)) +
        geom_boxplot(show.legend = FALSE) +
        ylab(paste0("INB (", intToUtf8(163), ")")) +
        xlab(paste0("LTBI unit test cost (", intToUtf8(163), ")")) +
        theme_bw() +
        theme(text = element_text(size = 30)) +
        geom_hline(yintercept = 0, linetype = "dashed")
      )

  }else if (!oneway) {

    # other_variable <- names(design_mat)[!names(design_mat) %in% c("scenario", "Agree_to_Screen_cost")]
    other_variable <- names(design_mat)[!names(design_mat) %in% c("scenario", "Agree_to_Screen_cost", "Complete_Treatment_p")]

    dat2 <-
      t(bcea$ib[bcea$k == wtp_threshold, , ]) %>%
      cbind.data.frame(design_mat, .) %>%
      melt.data.frame(id.vars = names(design_mat)) %>%
      dplyr::rename(INB = value) %>%
      dplyr::mutate(Agree_to_Screen_cost = factor(Agree_to_Screen_cost),
                    Start_Treatment_p = factor(Start_Treatment_p),
                    Complete_Treatment_p = factor(Complete_Treatment_p))

    dat2[ ,other_variable] <- factor(dat2[ ,other_variable])

    print(
      INMB_boxplot <-
        ggplot(dat2, aes_string(x = other_variable, y = "INB", group = "scenario", fill = "Complete_Treatment_p")) +
        geom_boxplot() +
        theme_bw() +
        labs(fill = "Complete \nTreatment\n") +
        # guides(fill = FALSE) +
        theme(text = element_text(size = 30)) +
        ylab(paste0("INB (", intToUtf8(163), ")")) +
        xlab(gsub(x = other_variable, "_|[_p]$", " ")) +
        geom_abline(slope = 0, intercept = 0))
  }

  ggplot2::ggsave(file = filename,
                  plot = INMB_boxplot,
                  width = 30, height = 20, units = "cm")

  save(INMB_boxplot, file = paste(folders$plots$scenario, "INMB_boxplot.RData", sep = "/"))

  invisible(out)
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
