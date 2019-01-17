
#' ---
#' title: "LTBI screening model:
#' create environments for each set of global
#' policy parameter values as inputs"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---


# incidence_list ----------------------------------------------------------
## WHO incidence groups: subsets
# incidence_list <-
#   list(c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]"),
#        c("(150,250]", "(250,350]", "(350,1e+05]"),
#        c("(250,350]", "(350,1e+05]"))
# incidence_list <-
#  list(c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]"),
#                        c("(50,150]"),
#                        c("(150,250]"),
#                        c("(250,350]"))
## everyone only
incidence_list <-
  list(c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]"))
# incidence_list <-
#   list(c("(0,50]", "(50,150]", "(150,250]"),
#        c("(250,350]", "(350,1e+05]"))


# endpoints ---------------------------------------------------------------
# endpoints <- c("death", "exit uk")
endpoints <- "exit uk"
# endpoints <- "death"


# LTBI_test ---------------------------------------------------------------
# LTBI_test <- c("QFT_GIT", "QFT_plus", "TSPOT")


# treatment ---------------------------------------------------------------
# treatment <- c("LTBI_Tx_3mISORIF", "LTBI_Tx_6mISO")
treatment <- c("LTBI_Tx_3mISORIF")


# LTBI_test ---------------------------------------------------------------
LTBI_test <- "TSPOT"
# LTBI_test <- "QFT_plus"


create_and_save_policies(incidence_list,
                         endpoints,
                         LTBI_test,
                         treatment)
