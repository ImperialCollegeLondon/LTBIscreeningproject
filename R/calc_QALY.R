
#' Calculate QALYs using List of Utilities
#'
#' @param time_horizon
#' @param utility
#'
#' @return
#' @export
#' @seealso \code{\link{calc_QALY_CFR}}
#'
#' @examples
#'
calc_QALY <- function(time_horizon = NA,
                      utility = 0.9){

  QALY <- NULL

  if(is.na(time_horizon) | length(utility)>time_horizon) time_horizon <- length(utility)
  utility <- fillin_missing_utilities(utility, time_horizon)

  discountfactor <- make_discount()
  period <- c(numeric(time_horizon - 1) + 1, 0.5)

  for(i in seq_along(utility)){

    QALY <- QALY + (period[i] * utility[i] * discountfactor())
  }

  return(QALY)
}



fillin_missing_utilities <- function(utility, time_horizon){

  c(utility, rep(utility(length(utility)),
                 max(0, time_horizon - n.utility, na.rm = T)))
}


#' Calculate QALYs using List of Utilities For Population
#'
#' @param utility
#' @param time_horizons
#'
#' @return
#' @export
#' @seealso \code{\link{calc_QALY_CFR}}, \code{\link{calc_QALY}}
#'
#' @examples
#'
calc_QALY_population <- function(utility, time_horizons){

  QALY <- NA
  for (horizon in seq_along(time_horizons)){

    QALY[horizon] <- calc_QALY(time_horizon = horizon, utility)
  }

  return(QALY)
}



