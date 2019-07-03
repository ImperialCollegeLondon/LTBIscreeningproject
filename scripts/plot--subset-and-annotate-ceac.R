
#' ---
#' title: "LTBI screening model:
#' take a scenario subset and annotate ceac"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---


design <-
  pastef(folders$output$parent,
         "scenario_params_df.csv") %>%
  read.csv() %>%
  design_matrix()

grid_points <-
  design %>%
  dplyr::filter(Complete_Treatment_p %in% c(0.5, 0.75, 0.9),
                Start_Treatment_p %in% c(0.5, 0.75, 0.9))

scenario_keep <-
  grid_points %>%
  transmute(scenario) %>%
  unlist(use.names = FALSE)

ce_res <- combine_popmod_dectree_res(cohort, interv, popmod_res, dectree_res, folders)
ce_res0 <- ce_res

cols_keep <- colnames(ce_res$ce_incr$e) %in% c(0,scenario_keep)

ce_res$ce_incr$e <- ce_res$ce_incr$e[ ,cols_keep]
ce_res$ce_incr$c <- ce_res$ce_incr$c[ ,cols_keep]

bcea_incr <- bcea_incremental(ce_res$ce_incr)

out <- ceac_plot_and_save(bcea_incr, folders)


### include baseline

baseline <- design[design$Start_Treatment_p == 0.95 &
                   design$Complete_Treatment_p  == 0.75, 'scenario']

cols_baseline <- colnames(ce_res0$ce_incr$e) %in% c(0, baseline)

ce_res$ce_incr$e <- ce_res0$ce_incr$e[ ,cols_baseline]
ce_res$ce_incr$c <- ce_res0$ce_incr$c[ ,cols_baseline]

bcea_incr <- bcea_incremental(ce_res$ce_incr)

out <-
  out + geom_line(data = data.frame(x = bcea_incr$k, y = bcea_incr$ceac),
                  aes(x = x, y = y),
                  inherit.aes = FALSE,
                  colour = 'red',
                  lwd = 1.2)

### annotate with scenario number

# annotate_coords <-
#   out$data %>%
#   dplyr::filter(ceac >= 0.73) %>%
#   group_by(comparison) %>%
#   slice(which.min(ceac))

# number each curve by scenario number
# two different methods

# label_data <- out$data[out$data$k == 40000, ]
# out <-
#   out + geom_text(data = label_data,
#                   aes(label = comparison, x = k, y = ceac, hjust = 1, vjust = -0.01),
#                   inherit.aes = F)

annotate_coords <-
  rbind(
    out$data %>%
      dplyr::filter(comparison %in% c(9,8,7),
                    ceac > 0.7) %>%
      group_by(comparison) %>%
      slice(which.min(ceac)),
    out$data %>%
      dplyr::filter(comparison %in% c(6,5,4),
                    ceac > 0.50,
                    ceac < 0.65) %>%
      group_by(comparison) %>%
      slice(which.min(ceac)),
    out$data %>%
      dplyr::filter(comparison %in% c(3,2,1),
                    ceac > 0.10,
                    ceac < 0.5) %>%
      group_by(comparison) %>%
      slice(which.min(ceac)))



out <-
  out + annotate("text",
                 x = annotate_coords$k - 500,
                 y = annotate_coords$ceac,
                 label = annotate_coords$comparison,
                 cex = 5)
out


## save
filename <- paste(folders$plots$scenario, "ceac.png", sep = "/")
ggplot2::ggsave(file = filename, plot = out,
                width = 20, height = 20, units = "cm")
save(out, file = paste(folders$plots$scenario, "ceac.RData", sep = "/"))
