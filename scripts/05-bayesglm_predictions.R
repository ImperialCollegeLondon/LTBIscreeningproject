#*******************************************************
# project: LTBI screening
# N Green
# Oct 2017
#
# 05-bayesglm_predictions


N.sim <-  10#00 #N.mc

# simulate from fitted model
lm.sim <- arm::sim(lm_multi$`20000`, n = N.sim)
coef.sim <- coef(lm.sim)

pred_sim_20000 <- matrix(NA,
                         ncol = nrow(coef.sim),
                         nrow = nrow(plot_data)/2)

pred_sim_20000_wide <- plot_data %>% select(-pred)

for (i in seq_len(N.sim)) {

  # replace with posterior sample
  lm_multi$`20000`$coefficients <- coef.sim[i, ]

  random_effect <- rnorm(n = nrow(plot_data), mean = 0, sd = lm.sim@sigma[i])

  plot_data$pred <- predict(lm_multi$`20000`, newdata = plot_data, type = "response") + random_effect

  pred_sim_20000_wide[ ,paste0("pred", i)] <- plot_data$pred

  pred_sim_20000[ ,i] <-
    tidyr::spread(plot_data, policy, pred) %>%
    arrange(Effective, Agree, Complete, Start) %>%
    transmute(screened - statusquo) %>% unlist()
}

# probability cost-effective
out_sim_20000 <-
  data.frame(plot_data_20000,
             prob_CE = apply(pred_sim_20000, 1,
                             function(x) sum(x > 0)/ncol(pred_sim_20000)),
             pc_5 = apply(pred_sim_20000, 1,
                         function(x) quantile(x, probs = 0.05)),
             pc_50 = apply(pred_sim_20000, 1,
                         function(x) quantile(x, probs = 0.5)),
             pc_95 = apply(pred_sim_20000, 1,
                          function(x) quantile(x, probs = 0.95)))


# plots -------------------------------------------------------------------

## probabilty cost-effective £20,000
s1 <- lattice::levelplot(prob_CE ~ Agree * Effective, subset(out_sim_20000, Start == 50 & Complete == 50),
                         xlab = "Agree (%)", ylab = "Effective (%)",
                         at = seq(0, 1, 0.01),
                         main = "Start = 50 & Complete = 50",
                         col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
s2 <- lattice::levelplot(prob_CE ~ Agree * Effective, subset(out_sim_20000, Start == 50 & Complete == 100),
                         xlab = "Agree (%)", ylab = "Effective (%)",
                         at = seq(0, 1, 0.01),
                         main = "Start = 50 & Complete = 100",
                         col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
s3 <- lattice::levelplot(prob_CE ~ Agree * Effective, subset(out_sim_20000, Start == 100 & Complete == 50),
                         xlab = "Agree (%)", ylab = "Effective (%)",
                         at = seq(0, 1, 0.01),
                         main = "Start = 100 & Complete = 50",
                         col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
s4 <- lattice::levelplot(prob_CE ~ Agree * Effective, subset(out_sim_20000, Start == 100 & Complete == 100),
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


# wtp = £30,000 -----------------------------------------------------------

lm.sim <- arm::sim(lm_multi$`30000`, n = N.sim)
coef.sim <- coef(lm.sim)

pred_sim_30000 <- matrix(NA,
                         ncol = nrow(coef.sim),
                         nrow = nrow(plot_data)/2)

pred_sim_30000_wide <- plot_data %>% select(-pred)

for (i in 1:nrow(coef.sim)) {

  # replace with posterior sample
  lm_multi$`30000`$coefficients <- coef.sim[i, ]

  random_effect <- rnorm(n = nrow(plot_data),
                         mean = 0,
                         sd = lm.sim@sigma[i])

  plot_data$pred <- predict(lm_multi$`30000`, newdata = plot_data, type = "response") + random_effect

  pred_sim_30000_wide[ ,paste0("pred", i)] <- plot_data$pred

  pred_sim_30000[ ,i] <-
    tidyr::spread(plot_data, policy, pred) %>%
    arrange(Effective, Agree, Complete, Start) %>%
    transmute(screened - statusquo) %>% unlist()
}

out_sim_30000 <-
  data.frame(plot_data_30000,
             prob_CE = apply(pred_sim_30000, 1,
                             function(x) sum(x > 0)/ncol(pred_sim_30000)),
             pc_5 = apply(pred_sim_30000, 1,
                          function(x) quantile(x, probs = 0.05)),
             pc_50 = apply(pred_sim_30000, 1,
                           function(x) quantile(x, probs = 0.5)),
             pc_95 = apply(pred_sim_30000, 1,
                           function(x) quantile(x, probs = 0.95)))

## probabilty cost-effective
s1 <- lattice::levelplot(prob_CE ~ Agree * Effective, subset(out_sim_30000, Start == 50 & Complete == 50),
                         xlab = "Agree (%)", ylab = "Effective (%)",
                         at = seq(0, 1, 0.01),
                         main = "Start = 50 & Complete = 50",
                         col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
s2 <- lattice::levelplot(prob_CE ~ Agree * Effective, subset(out_sim_30000, Start == 50 & Complete == 100),
                         xlab = "Agree (%)", ylab = "Effective (%)",
                         at = seq(0, 1, 0.01),
                         main = "Start = 50 & Complete = 100",
                         col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
s3 <- lattice::levelplot(prob_CE ~ Agree * Effective, subset(out_sim_30000, Start == 100 & Complete == 50),
                         xlab = "Agree (%)", ylab = "Effective (%)",
                         at = seq(0, 1, 0.01),
                         main = "Start = 100 & Complete = 50",
                         col.regions = rainbow(n = 100, start = 3/6, end = 1/6))#topo.colors(100))
s4 <- lattice::levelplot(prob_CE ~ Agree * Effective, subset(out_sim_30000, Start == 100 & Complete == 100),
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

filename <- paste(plots_folder_scenario, "prob_CE_grid_30000.png", sep = "/")
ggsave(file = filename, plot = g, width = 30, height = 20, units = "cm")


# output table ------------------------------------------------------------

regn_CE_grid <- merge(out_sim_20000, out_sim_30000,
                 by = c("Agree", "Start", "Complete", "Effective"),
                 suffixes = c(".2k", ".3k"))

try(
  write.csv(x = regn_CE_grid,
            file = paste(diroutput, "regn_CE_grid_table.csv", sep = "/")))




# specific scenario line graphs -------------------------------------------

df <- dplyr::filter(regn_CE_grid,
                    Agree == 72, Start == 100, Complete == 100)
df_long <- reshape::melt(df, measure.vars = c("prob_CE.3k","prob_CE.2k"), variable_name = "WTP")


print(
  ggplot(aes(x = Effective, y = prob_CE.3k), data = df_long) +
  geom_smooth(aes(x = Effective, y = value, fill = WTP), se = FALSE) +
  ylab("Probability cost-effective") +
  geom_hline(yintercept = 0.5))

filename <- paste(plots_folder_scenario, "prob_CE_Effective_72-100-100.png", sep = "/")
ggsave(file = filename, width = 30, height = 20, units = "cm")


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
