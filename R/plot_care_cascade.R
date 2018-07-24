
#' Plot care cascade
#'
#' @param data_folder string
#' @param plots_folder string
#' @param prob_or_num Probabilities or absolute numbers
#' @param box_plot default: FALSE
#'
#' @return
#' @export
#'
#' @examples
#'
#' plot_care_cascade(parent_folder,
#'                   prob_or_num = "prob")
#'
plot_care_cascade <- function(data_folder,
                              plots_folder,
                              prob_or_num,
                              box_plot = FALSE) {

  file_name <-
    if (prob_or_num == "prob") {
      "combined_prob_subset_dectree.csv"
    } else {
      "combined_all_subsets.csv"
    }

  cascade_data <- readr::read_csv(pastef(data_folder, file_name))

  cascade_data$scenario <- as.factor(cascade_data$scenario)
  names(cascade_data)[names(cascade_data) == "L95"] <- "L95_1"
  names(cascade_data)[names(cascade_data) == "U95"] <- "U95_1"
  # names(cascade_data)[names(cascade_data) == "L95"] <- "L50_1"
  # names(cascade_data)[names(cascade_data) == "U95"] <- "U50_1"
  names(cascade_data)[names(cascade_data) == "mean"] <- "mean_1"

  # select columns to plot

  cascade_LTBI <-
    cascade_data %>%
    subset(variable %in% c("LTBI_completeTx","LTBI_positive","LTBI_startTx","LTBI_tests","p_LTBI_to_cured"))

  cascade_all <-
    cascade_data %>%
    subset(variable %in% c("LTBI_pre","tests","positive","startTx","completeTx","cured","LTBI_post"))

  # reorder
  cascade_all$variable <- factor(cascade_all$variable,
                           levels = c("LTBI_pre","tests","positive","startTx","completeTx","cured","LTBI_post"))

  cascade_LTBI$variable <- factor(cascade_LTBI$variable,
                            levels = c("LTBI_tests","LTBI_positive","LTBI_startTx","LTBI_completeTx","p_LTBI_to_cured"))

  const_cols <- grepl(x = names(cascade_LTBI), pattern = "X1|scenario|variable")

  for (i in policies) {

    cols_policy <- const_cols | grepl(x = names(cascade_LTBI),
                                      pattern = paste0("._", i))

    dat <- cascade_LTBI[ ,cols_policy]
    gg_care_cascade(dat, plots_folder, prob_or_num, policies_ls[i], "LTBI", box_plot)

    dat <- cascade_all[ ,cols_policy]
    gg_care_cascade(dat, plots_folder, prob_or_num, policies_ls[i], "all", box_plot)

  }

}


#' gg_care_cascade
#'
#' @param dat
#' @param plots_folder
#' @param prob_or_num
#' @param policy_name
#' @param grp
#' @param box_plot
#'
#' @return
#' @export
#'
#' @examples
#'
gg_care_cascade <- function(dat,
                            plots_folder = NA,
                            prob_or_num = "prob",
                            policy_name = NA,
                            grp = NA,
                            box_plot = FALSE) {

  names(dat) <- gsub(pattern = "_.+",
                     replacement = "",
                     names(dat))

  p <-
    if (box_plot) {

      ggplot(dat, aes(x = factor(variable), fill = scenario, col = scenario)) +
        geom_boxplot(aes(lower = mean, middle = mean, upper = mean,
                         ymin = L95, ymax = U95),
                     stat = "identity")
    } else {

      ggplot(dat, aes(x = variable, y = mean, fill = scenario)) +
        geom_bar(stat = "identity", color = "white",
                 position = position_dodge()) +
        geom_errorbar(aes(ymin = L95, ymax = U95), width = 0.2,
                      position = position_dodge(0.9))
    }

  p <-
    p +
    # ggplot2::ylim(0, 1) +
    theme_bw() +
    ylab('Number in cohort intended for screening') +
    xlab('')

  if (!is.na(plots_folder)) {

  ggplot2::ggsave(p,
                  file = paste(plots_folder, policy_name,
                               paste0(prob_or_num, "_cascade_", grp, ".png"), sep = "/"),
                  width = 30, height = 20, units = "cm")
  } else {
    return(p)
  }
}

