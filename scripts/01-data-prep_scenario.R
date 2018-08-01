
#' ---
#' title: "TBI screening model:
#' prep scenario decision tree cost & QALY data"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---


# file_tag <- "_high-low"
# file_tag <- "_baseline"
file_tag <- "_effective"
# file_tag <- "_test"
# file_tag <- "_oneway"
# file_tag <- "_fullfactorial"

create_and_save_scenarios(file_tag)


# convert to percentages and simplify names
design_matrix <-
  scenario_parameter_p %>%
  mutate_at(vars(-scenario),
            funs(.*100)) %>%
  set_names(~sub(' Treatment', '', .x)) %>%
  set_names(~sub(' to Screen', '', .x))

save(design_matrix, file = paste0(here::here(), "/data/design_matrix.RData"))
