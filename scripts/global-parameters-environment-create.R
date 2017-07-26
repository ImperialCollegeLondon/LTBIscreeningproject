#
# LTBI screening
# N Green
#
# create environments for each set of global
# parameter values in scenarios as inputs
#

incidence_list <-  list(c("(0,50]", "(50,150]", "(150,250]", "(250,350]", "(350,1e+05]"),
                        c("(150,250]", "(250,350]", "(350,1e+05]"),
                        c("(250,350]", "(350,1e+05]"))

for (s in c("QFT", "TST")) {
  for (m in c(0,5)) {
    for (i in 1:length(incidence_list)) {

      environ_name <- paste(s, "LoS", m, "incidgrp", i, sep = "_")

      assign(x = environ_name, value = new.env())
      assign(x = "min_screen_length_of_stay", value = m, envir = eval(parse(text = environ_name)))
      assign(x = "study", value = s, envir = eval(parse(text = environ_name)))
      assign(x = "incidence_grps_screen", value = incidence_list[[i]], envir = eval(parse(text = environ_name)))
    }
  }
}

rm(environ_name, i, m, s, incidence_list)

save.image(file = "data/global-parameters-scenarios.RData")

ls_global_params_scenarios <- ls()
save(ls_global_params_scenarios, file = "data/global-parameters-scenarios_ls.RData")

