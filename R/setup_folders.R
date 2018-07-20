
#' setup_folders
#'
#' @param policy_name
#' @param interv
#'
#' @return
#' @export
#'
#' @examples
#'
setup_folders <- function(policy_name,
                          interv) {

  root_wd <- getwd()
  on.exit(setwd(root_wd))
  folders <- list()

  folders$output$scenario <- diroutput(policy_name, interv)

  dir.create(folders$output$scenario, showWarnings = FALSE, recursive = TRUE)

  folders$plots$parent <- system.file("output", "plots",
                              package = "LTBIscreeningproject")

  folders$plots$scenario <- sprintf("%s/%s", folders$plots$parent, policy_name)
  dir.create(folders$plots$scenario, showWarnings = FALSE)

  setwd(folders$output$scenario)

  file.copy(from = paste(root_wd, "data/scenario_parameters_df.csv", sep = "/"),
            to = "../scenario_parameters_df.csv")

  file.copy(from = paste(root_wd, "data/policies-inputs.csv", sep = "/"),
            to = "../policies-inputs.csv")

  return(folders)
}
