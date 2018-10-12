
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


## WHO incidence groups: subsets
incidence_list <- list(c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]"),
                       c("(150,250]", "(250,350]", "(350,1e+05]"),
                       c("(250,350]", "(350,1e+05]"))

## WHO incidence groups: mutually exclusive
# incidence_list <- list(c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]"),
#                        c("(50,150]"),
#                        c("(150,250]"),
#                        c("(250,350]"))

## everyone
# incidence_list <- list(c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]"))

endpoints <- c("death", "exit uk")

# LTBI_test <- c("QFT_GIT", "QFT_plus", "TSPOT")
# treatment <- c("LTBI_Tx_3mISORIF", "LTBI_Tx_6mISO")


## test:
# incidence_list <- list(c("(0,50]", "(50,150]", "(150,250]"),
#                        c("(250,350]", "(350,1e+05]"))
# endpoints <- c("death")

# LTBI_test <- c("TSPOT")
LTBI_test <- c("QFT_plus")

treatment <- c("LTBI_Tx_3mISORIF")

create_and_save_policies(incidence_list,
                         endpoints,
                         LTBI_test,
                         treatment)
