#
# create table of country TB prevalence
# and incidence by year
#
#


temp2 <-
  WHO_incid_by_country_all_years %>%
  dplyr::filter(Year %in% c('2009', '2015'))

temp2 <- temp2[ ,c(1,2,5)]
names(temp2) <- c("country", "year", "e_inc_100k")
temp2$year <- paste0(temp2$year, "_inc")

e_inc_100k <- reshape::cast(temp2, formula = country~year, value = "e_inc_100k")


temp3 <-
  UN_TB_prev_country_years %>%
  dplyr::filter(Year %in% c('2009', '2013'))


names(temp3) <- c("country", "year", "e_prev_100k")
temp3$year <- paste0(temp3$year, "_prev")

e_prev_100k <- reshape::cast(temp3, formula = country~year, value = "e_prev_100k")

e_prev_inc_country <- merge(e_inc_100k, e_prev_100k, by = "country")

write.csv(e_prev_inc_country, file = "data/e_prev_inc_country.csv")
