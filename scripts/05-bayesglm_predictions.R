#' ---
#' title: "LTBI screening model:
#' 05-bayesglm_predictions"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---



# if (is.na(newdata)) {
  newdata <- read.csv(here::here("data", "predict_newdata.csv"),
                      stringsAsFactors = TRUE)

  load(paste0(folders$output$scenario, "/lm_multi_all.RData"))
# }

newdata <- rm_redundant_covariates(lm_fit, newdata)

num_scenarios <- nrow(newdata)/2

n_sim <-  1000 #N.mc


out_sim <- lapply(lm_fit, bayes_predict, newdata, n_sim)






# plots -------------------------------------------------------------------

## probabilty cost-effective £20,000
s1 <- lattice::levelplot(prob_CE ~ Start_Treatment_p * Complete_Treatment_p,
                         out_sim[[1]],
                         # subset(out_sim_20000, Start == 50 & Complete == 50),
                         xlab = "Start (%)", ylab = "Complete (%)",
                         at = seq(0, 1, 0.01),
                         # main = "Start = 50 & Complete = 50",
                         col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
s2 <- lattice::levelplot(prob_CE ~ Agree * Effective, subset(out_sim$20000, Start == 50 & Complete == 100),
                         xlab = "Agree (%)", ylab = "Effective (%)",
                         at = seq(0, 1, 0.01),
                         main = "Start = 50 & Complete = 100",
                         col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
s3 <- lattice::levelplot(prob_CE ~ Agree * Effective, subset(out_sim$20000, Start == 100 & Complete == 50),
                         xlab = "Agree (%)", ylab = "Effective (%)",
                         at = seq(0, 1, 0.01),
                         main = "Start = 100 & Complete = 50",
                         col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
s4 <- lattice::levelplot(prob_CE ~ Agree * Effective, subset(out_sim$20000, Start == 100 & Complete == 100),
                         xlab = "Agree (%)", ylab = "Effective (%)",
                         at = seq(0, 1, 0.01),
                         main = "Start = 100 & Complete = 100",
                         col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))

print(
  grid.arrange(arrangeGrob(s1, s2),
               arrangeGrob(s3, s4),
               ncol = 2)
)

g <- arrangeGrob(s1, s2, s3, s4, nrow = 2)

filename <- paste(plots_folder_scenario, "prob_CE_grid_20000.png", sep = "/")
ggsave(file = filename, plot = g, width = 30, height = 20, units = "cm")





# output array ------------------------------------------------------------

regn_CE_grid <- merge(x = out_sim_20000,
                      y = out_sim_30000,
                      by = c("Agree", "Start", "Complete", "Effective"),
                      suffixes = c(".2k", ".3k"))

try(
  write.csv(x = regn_CE_grid,
            file = paste(diroutput, "regn_CE_grid_table.csv", sep = "/")))




# specific scenario line graphs -------------------------------------------

# df <- dplyr::filter(regn_CE_grid,
#                     Agree == 72, Start == 100, Complete == 100)
# df_long <- reshape::melt(df,
#                          measure.vars = c("prob_CE.3k","prob_CE.2k"),
#                          variable_name = "WTP")
#
#
# print(
#   ggplot(aes(x = Effective, y = prob_CE.3k), data = df_long) +
#   geom_smooth(aes(x = Effective, y = value, fill = WTP), se = FALSE) +
#   ylab("Probability cost-effective") +
#   geom_hline(yintercept = 0.5))
#
# filename <- paste(plots_folder_scenario, "prob_CE_Effective_72-100-100.png", sep = "/")
# ggsave(file = filename, width = 30, height = 20, units = "cm")


# ggplot(data = df, aes(x = Effective, y = INMB.3k)) +#, group=t, colour=t)) +
#   geom_ribbon(aes(ymin = pc_5.2k, ymax = pc_95.2k, fill = "blue"), alpha = 0.5) +#, fill = t) +
#   geom_ribbon(aes(ymin = pc_5.3k, ymax = pc_95.3k, fill = "red"), alpha = 0.5) + #, fill = t)
#   geom_line(aes(x = Effective, y = INMB.2k), data = df) +
#   geom_line(aes(x = Effective, y = INMB.3k), data = df)

##TODO:
# plot(x, xlim = c(-1, 1), ylim = c(-1, 1), xlab = "x", ylab = "y", type = "n")
# denstrip(x = c(coef(M1.sim)[ ,23]), at = 0)
#
# # plots
# x11()
# par(mfrow = c(5, 6))
# for (i in 1:ncol(coef.sim)) {
#
#   hist(coef(M1.sim)[ ,i], main = "", xlab = colnames(coef.sim)[i], xlim = c(-0.5, 0.7))
# }
