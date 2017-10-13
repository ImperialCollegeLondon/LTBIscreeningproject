#
# 05-stan_predictions
#


dm_20000 <- subset(sim_matrix, wtp == 20000)

post_20000 <- stan_lm(formula = nmb_formula,
                      data = dm_20000,
                      prior = R2(location = 0.2), chains = 2)

plot_data <- dplyr::filter(pred_data,
                           Start %in% c(50,100), Complete %in% c(50,100))

N.draws <- 1000

dm0_20000 <- dplyr::filter(plot_data, policy == "statusquo")
y0_rep <- posterior_predict(post_20000, newdata = dm0_20000, draws = N.draws) %>% t()

dm1_20000 <- dplyr::filter(plot_data, policy == "screened")
y1_rep <- posterior_predict(post_20000, newdata = dm1_20000, draws = N.draws) %>% t()


INMB_sim_20000 <- data.frame(y1_rep - y0_rep)


out_sim_20000 <-
  data.frame(dm0_20000,
             prob_CE = apply(INMB_sim_20000, 1,
                             function(x) sum(x > 0)/ncol(INMB_sim_20000)),
             EINMB = apply(INMB_sim_20000, 1, mean),
             pc_5 = apply(INMB_sim_20000, 1,
                          function(x) quantile(x, probs = 0.05)),
             pc_50 = apply(INMB_sim_20000, 1,
                           function(x) quantile(x, probs = 0.5)),
             pc_95 = apply(INMB_sim_20000, 1,
                           function(x) quantile(x, probs = 0.95)))


# plots -------------------------------------------------------------------

qplot(x = c(y1_rep - y0_rep), geom = "histogram", xlab = "Estimated average treatment effect")


# df <- data.frame(X = c(y1_rep - y0_rep))
# ggplot(df, aes(x = X)) +
#   geom_histogram(binwidth = 10, fill = "white", color = "black") +
#   geom_histogram(binwidth = 10, data = subset(df, X >= 0),
#                  colour = "black", fill = "grey") + theme_bw()


# coefficient plots -------------------------------------------------------

plot(post_20000, "areas") + xlim(-1, 2)


# probabilty cost-effective plot ------------------------------------------

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
#
# filename <- paste(plots_folder_scenario, "prob_CE_grid_20000.png", sep = "/")
# ggsave(file = filename, plot = g, width = 30, height = 20, units = "cm")



# wtp £30,000 -------------------------------------------------------------

dm_30000 <- subset(sim_matrix, wtp == 30000)

post_30000 <- stan_lm(formula = nmb_formula,
                      data = dm_30000,
                      prior = R2(location = 0.2), chains = 2)

plot_data <- dplyr::filter(pred_data,
                           Start %in% c(50,100), Complete %in% c(50,100))

dm0_30000 <- dplyr::filter(plot_data, policy == "statusquo")
y0_rep <- posterior_predict(post_30000, newdata = dm0_30000, draws = 1000) %>% t()

dm1_30000 <- dplyr::filter(plot_data, policy == "screened")
y1_rep <- posterior_predict(post_30000, newdata = dm1_30000, draws = 1000) %>% t()

# qplot(x = c(y1_rep - y0_rep), geom = "histogram", xlab = "Estimated ATE")

INMB_sim_30000 <- data.frame(y1_rep - y0_rep)


out_sim_30000 <-
  data.frame(dm0_30000,
             prob_CE = apply(INMB_sim_30000, 1,
                             function(x) sum(x > 0)/ncol(INMB_sim_30000)),
             EINMB = apply(INMB_sim_30000, 1, mean),
             pc_5 = apply(INMB_sim_30000, 1,
                          function(x) quantile(x, probs = 0.05)),
             pc_50 = apply(INMB_sim_30000, 1,
                           function(x) quantile(x, probs = 0.5)),
             pc_95 = apply(INMB_sim_30000, 1,
                           function(x) quantile(x, probs = 0.95)))


# coefficient plots -------------------------------------------------------

plot(post_30000, "areas") + xlim(-1, 2)


# probabilty cost-effective plot ------------------------------------------

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
#
# filename <- paste(plots_folder_scenario, "prob_CE_grid_30000.png", sep = "/")
# ggsave(file = filename, plot = g, width = 30, height = 20, units = "cm")
