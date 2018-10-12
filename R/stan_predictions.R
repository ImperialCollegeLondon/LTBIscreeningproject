
#' stan_predictions
#'
#' @param ce_res
#' @param folders
#' @param use_newdata
#'
#' @return
#' @export
#'
#' @examples
#'
stan_predictions <- function(ce_res,
                             folders,
                             use_newdata = TRUE) {

  library(rstanarm)

  design_mat <-
    pastef(folders$output$parent,
           "scenario_params_df.csv") %>%
    read.csv() %>%
    design_matrix() %>%
    remove_cols_constant_vars()

  vars <- string_sum_covariates(design_mat)

  interactions <-
    paste0("(", vars, ")^2")

  nmb_formula <- as.formula(paste("NMB ~ type *", interactions))

  nmb_mat <- nmb_matrix(ce_res$ce1, ce_res$ce0, folders)

  ##TODO: move this so only run once...
  stan_fit <- lapply(nmb_mat,
                     function(x) rstanarm::stan_lm(formula = nmb_formula,
                                                   data = x,
                                                   prior = R2(location = 0.2),
                                                   chains = 2,
                                                   iter = 10000))
  if (use_newdata) {newdata <- NA
  } else {newdata <- nmb_mat[[1]][nmb_mat[[1]]$runs == 1, ]}

  if (is.na(newdata)) {
    newdata <- read.csv(here::here("data", "predict_newdata.csv"),
                        stringsAsFactors = TRUE)
  }

  newdata <- rm_redundant_covariates(stan_fit, newdata)

  n_draws <- 10000

  stan_preds <- lapply(stan_fit,
                       stan_predict,
                       newdata = newdata,
                       n_draws)

  save(stan_preds, file = pastef(folders$output$scenario, "stan_preds.RData"))

  cov_names <- names(design_mat)[names(design_mat) != 'scenario']
  stan_preds_df <- plyr::join_all(stan_preds, by = cov_names, type = 'left')
  write.csv(stan_preds_df, file = pastef(folders$output$scenario, "stan_preds_df.csv"))

  stan_preds
}


