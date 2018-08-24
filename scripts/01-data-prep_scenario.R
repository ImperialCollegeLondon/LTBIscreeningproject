
#' ---
#' title: "LTBI screening model:
#' prep scenario decision tree cost & QALY data"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---


## grid of values/design
# file_tag <- "_baseline"
# file_tag <- "_effective-cost"
# file_tag <- "_Excel_test"
# file_tag <- "_ff"
# file_tag <- "_ff_current_tech"
# file_tag <- "_ff_future_effic_GBP25"
# file_tag <- "_ff_future_effic_GBP50"
# file_tag <- "_ff_future_effic_GBP100"
# file_tag <- "_ff_future_test"
# file_tag <- "_future_test"
# file_tag <- "_tornado"
file_tag <- "_lhc"
# file_tag <- "_testcost"

create_and_save_scenarios(file_tag)

