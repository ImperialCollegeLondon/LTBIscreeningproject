
#' policy_interv
#'
#' Set the intervention parameter values within an evinonment.
#'
#' @param policy_name string
#' @param interv list
#'
#' @return
#' @export
#'
#' @examples
#'
policy_interv <- function(policy_name,
                          interv) {

  diroutput <- diroutput(policy_name, interv)

  get_current_policy <- get_from_envir(policy_name)

  interv$incidence_grps_screen <- get_current_policy("incidence_grps_screen")
  interv$min_screen_length_of_stay <- get_current_policy("min_screen_length_of_stay")
  interv$ENDPOINT_cost <- get_current_policy("ENDPOINT_cost")
  interv$ENDPOINT_QALY <- get_current_policy("ENDPOINT_QALY")
  interv$LTBI_test <- get_current_policy("LTBI_test")
  interv$treatment <- get_current_policy("treatment")

  message(sprintf("[ policy level parameters ]\n
                  policy: %s\n
                  WHO groups: %s\n
                  min stay: %s\n
                  cost endpoint: %s\n
                  QALY endpoint: %s\n
                  test: %s\n
                  treatment: %s",
                  green(policy_name),
                  green(paste(interv$incidence_grps_screen, collapse = "")),
                  green(interv$min_screen_length_of_stay),
                  green(interv$ENDPOINT_cost),
                  green(interv$ENDPOINT_QALY),
                  green(interv$LTBI_test),
                  green(interv$treatment)))

  save(interv, file = pastef(diroutput, "interv.RData"))

  return(interv)
}
