#' ---
#' title: "LTBI screening model:
#' 05-stan_predictions"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---

library(rstanarm)

nmb_mat <- nmb_matrix(ce_res$ce1, ce_res$ce0, folders)

# nmb_formula <- as.formula(NMB ~ type * (Complete_Treatment_p + Start_Treatment_p)^2)
nmb_formula <- as.formula(NMB ~ type * Agree_to_Screen_cost)

stan_fit <- lapply(nmb_mat,
                   function(x) rstanarm::stan_lm(formula = nmb_formula,
                                                 data = x,
                                                 prior = R2(location = 0.2),
                                                 chains = 2))

n_draws <- 200

# newdata <- read.csv(here::here("data", "predict_newdata.csv"),
#                     stringsAsFactors = TRUE)
#
# newdata <- rm_redundant_covariates(stan_fit, newdata)

# unit test cost only
newdata <- data.frame(type = c(rep("statusquo",3),
                               rep("screened",3)),
                      Agree_to_Screen_cost = c(25,50,100,
                                               25,50,100))

out_sim <- lapply(stan_fit,
                  stan_predict,
                  newdata = newdata,
                  n_draws)


# coefficient plots -------------------------------------------------------

plot(stan_fit[[3]], "areas") + xlim(-1, 2)


# probabilty cost-effective plot ------------------------------------------

s1 <- lattice::levelplot(prob_CE ~ Start_Treatment_p * Complete_Treatment_p,
                         out_sim[[1]],
                         # subset(out_sim_20000, Start == 50 & Complete == 50),
                         xlab = "Start (%)", ylab = "Complete (%)",
                         at = seq(0, 1, 0.01))
# main = "Start = 50 & Complete = 50",
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

# filename <- paste(plots_folder_scenario, "prob_CE_grid_20000.png", sep = "/")
# ggsave(file = filename, plot = g, width = 30, height = 20, units = "cm")

