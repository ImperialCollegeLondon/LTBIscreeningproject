
#' form name of output folder
#'
#' @param policy_name
#' @param interv
#'
#' @return
#' @export
#'
diroutput <- function(policy_name,
                      interv) {

  # create permanent output folder
  parent_folder <- sprintf("ext-data/%d_to_%d_in_%s",
                           min(interv$screen_age_range),
                           max(interv$screen_age_range),
                           interv$year_cohort)

  dir <- sprintf("%s/%s", parent_folder, policy_name)

  return(dir)
}
