
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

  ext_data <- system.file("ext-data",
                          package = "LTBIscreeningproject")

  # create permanent output folder
  parent_folder <- sprintf("%s/%d_to_%d_in_%s",
                           ext_data,
                           min(interv$screen_age_range),
                           max(interv$screen_age_range),
                           interv$year_cohort)

  dir_policy <- sprintf("%s/%s", parent_folder, policy_name)

  return(dir_policy)
}
