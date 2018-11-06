
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
  dplyr::filter(Complete_Treatment_p %in% c(0.5,0.75,0.9),
                Start_Treatment_p %in% c(0.5,0.75,0.9))
scenario_keep <-
  grid_points %>%
  transmute(scenario) %>% unlist(use.names = F)

ce_res$ce_incr$e <- ce_res$ce_incr$e[ ,colnames(ce_res$ce_incr$e) %in% c(0,scenario_keep)]
ce_res$ce_incr$c <- ce_res$ce_incr$c[ ,colnames(ce_res$ce_incr$e) %in% c(0,scenario_keep)]

bcea_incr <- bcea_incremental(ce_res$ce_incr)

ceac_plot_and_save(bcea_incr, folders)

coords <-
  out$data %>%
  dplyr::filter(ceac >= 0.73) %>%
  group_by(comparison) %>%
  slice(which.min(ceac))

out + annotate("text",
               x = coords$k - 500,
               y = coords$ceac,
               label = coords$comparison,
               cex = 5)
