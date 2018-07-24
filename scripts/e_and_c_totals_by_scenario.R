#
# LTBI screening
# N Green
#
# e_and_c_totals_by_scenario.R


flder <-
  list.dirs(data_folder)[-1] %>%
  sort()

load(paste0(flder[1], "/e_and_c_totals.RData"))

n.scenarios <- ncol(c.total) - 1
N.mc <- nrow(c.total)

e.total_scenario <- data.frame(matrix(data = 0,
                                      nrow = N.mc,
                                      ncol = n.scenarios),
                               check.names = FALSE)

c.total_scenario <- e.total_scenario


for (i in seq_along(flder)) {

  load(paste0(flder[i], "/e_and_c_totals.RData"))

  e.total_scenario <- map2(e.total_scenario, as.data.frame(e.total)[ ,-1], cbind)
  c.total_scenario <- map2(c.total_scenario, as.data.frame(c.total)[ ,-1], cbind)
}

save(e.total_scenario,
     c.total_scenario,
     file = paste0(data_folder, "/e_and_c_totals_by_scenario.RData"))




