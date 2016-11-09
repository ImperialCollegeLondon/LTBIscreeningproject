
#' Calculate QALYs Using Vector of Utilities
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

  if(is.na(time_horizon) | length(utility) > time_horizon) time_horizon <- length(utility)
  utility <- fillin_missing_utilities(utility, time_horizon)

  QALY <- 0
  discountfactor <- make_discount()
  period <- c(numeric(time_horizon - 1) + 1, 0.5)

  for(i in seq_along(utility)){

    QALY <- QALY + (period[i] * utility[i] * discountfactor())
  }

  return(QALY)
}



#' Fill-in Missing Trailing Utilities
#'
#' @param utility
#' @param time_horizon
#'
#' @return
#' @export
#'
fillin_missing_utilities <- function(utility, time_horizon){

  n.utility <- length(utility)
  c(utility, rep(utility[n.utility],
                 max(0, time_horizon - n.utility, na.rm = T)))
}


#' Calculate QALYs Using Vector of Utilities For Population
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
  for (i in seq_along(time_horizons)){

    QALY[i] <- calc_QALY(time_horizon = time_horizons[i], utility)
  }

  return(QALY)
}



