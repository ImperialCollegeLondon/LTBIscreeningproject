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


##TODO: use previous regression code here...
nmb_mat <- nmb_matrix(ce_res$ce1, ce_res$ce0, folders)

nmb_formula <- as.formula(NMB ~ type * (Complete_Treatment_p + Start_Treatment_p)^2)
# nmb_formula <- as.formula(NMB ~ type * Agree_to_Screen_cost)

stan_fit <- lapply(nmb_mat,
                   function(x) rstanarm::stan_lm(formula = nmb_formula,
                                                 data = x,
                                                 prior = R2(location = 0.2),
                                                 chains = 2))

n_draws <- 1000

newdata <- read.csv(here::here("data", "predict_newdata.csv"),
                    stringsAsFactors = TRUE)

newdata <- rm_redundant_covariates(stan_fit, newdata)

# # unit test cost only
# newdata <- data.frame(type = c(rep("statusquo",3),
#                                rep("screened",3)),
#                       Agree_to_Screen_cost = c(25,50,100,
#                                                25,50,100))

stan_preds <- lapply(stan_fit,
                     stan_predict,
                     newdata = newdata,
                     n_draws)

save(stan_preds, file = pastef(folders$output$scenario, "stan_preds.RData"))


# plots -------------------------------------------------------

plot(stan_preds$`20000`, "areas") + xlim(-1, 2)

prob_ce_gridplot(stan_preds$`20000`, folders = folders)

