
#' ---
#' title: "LTBI screening model:
#' latin hypercube sample input parameter values"
#' http://stat.ethz.ch/pipermail/r-help/2007-January/124143.html
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---


library(lhs)

var_names <-
  c("Agree_to_Screen_p",
    "Agree_to_Screen_cost",
    "Sensitivity",
    "Specificity",
    "Start_Treatment_p",
    "Complete_Treatment_p",
    "Effectiveness")

sample_size <- 10

lhc <-
  randomLHS(sample_size,
            length(var_names)) %>%
  `colnames<-`(var_names)

lhc[,'Agree_to_Screen_cost'] <-
  lhc[,'Agree_to_Screen_cost'] %>%
  qunif(1, 100)

write.csv(lhc, here::here("data", "lhc.csv"))

