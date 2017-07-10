
# use data from plot in Robs paper Lancet

incidence_mean <- read_csv("C:/Users/ngreen1/Dropbox/TB/LTBI/data/incidence-by-time-since-entry-mean.csv", col_names = FALSE)
incidence_lower95 <- read_csv("C:/Users/ngreen1/Dropbox/TB/LTBI/data/incidence-by-time-since-entry-lower95.csv", col_names = FALSE)
incidence_upper95 <- read_csv("C:/Users/ngreen1/Dropbox/TB/LTBI/data/incidence-by-time-since-entry-upper95.csv", col_names = FALSE)

incidence_mean$X1 %<>% round()
incidence_lower95$X1 %<>% round()
incidence_upper95$X1 %<>% round()

incidence_Lancet <-
  plyr::join_all(list(incidence_mean,
                      incidence_lower95,
                      incidence_upper95), by = "X1") %>%
  set_names(nm = c("year", "mean", "lower95", "upper95"))

save(incidence_Lancet, file = "data/incidence_Lancet.RData")
