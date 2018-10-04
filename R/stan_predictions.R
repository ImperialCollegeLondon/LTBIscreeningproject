
#' stan_predictions
#'
#' @param ce_res
#' @param folders
#' @param newdata
#'
#' @return
#' @export
#'
#' @examples
#'
stan_predictions <- function(ce_res,
                             folders,
                             newdata = TRUE) {

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
                                                   chains = 2))
  if (newdata) {newdata <- NA
  } else {newdata <- nmb_mat}

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

  stan_pred
}


