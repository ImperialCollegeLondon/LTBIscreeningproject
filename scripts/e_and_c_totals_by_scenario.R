#
# LTBI screening
# N Green
#
# e_and_c_totals_by_scenario.R


flder <- list.dirs(parent_folder)[-1]

e.total_scenario <- data.frame(matrix(data = 0,
                                      nrow = interv$N.mc,
                                      ncol = n.scenarios),
                               check.names = FALSE)

c.total_scenario <- data.frame(matrix(data = 0,
                                      nrow = interv$N.mc,
                                      ncol = n.scenarios),
                               check.names = FALSE)

for (i in seq_along(flder)) {

  load(paste0(flder[i], "/e_and_c_totals.RData"))

  e.total_scenario <- map2(e.total_scenario, as.data.frame(e.total)[ ,-1], cbind)
  c.total_scenario <- map2(c.total_scenario, as.data.frame(c.total)[ ,-1], cbind)
}

save(e.total_scenario,
     c.total_scenario,
     file = "ext-data/e_and_c_totals_by_scenario.RData")




