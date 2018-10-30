
# https://stackoverflow.com/questions/32034140/r-plotly-combine-bubble-and-chorpleth-map
#
# https://stackoverflow.com/questions/22625119/choropleth-world-map#

library(rworldmap)


TB_burden <- read.csv("C:/Users/ngreen1/Dropbox/TB/LTBI/R/LTBIscreeningproject/data/TB_burden_countries_2018-05-04.csv")
country_long_lat <- read.csv("C:/Users/ngreen1/Dropbox/TB/LTBI/R/LTBIscreeningproject/data/country-long-lat.csv")

TB_burden <- TB_burden[TB_burden$year == 2011, c("country", "e_inc_100k", "e_inc_num")]
country_long_lat <- country_long_lat %>% dplyr::select(-country)


gtdMap <- joinCountryData2Map(TB_burden,
                              nameJoinColumn = "country",
                              joinCode = "NAME")

mapDevice('x11')
mapCountryData(gtdMap,
               nameColumnToPlot = 'e_inc_100k',
               catMethod = 'fixedWidth',
               numCats = 100, mapTitle = "TB incidence per 100,000")


load("C:/Users/ngreen1/Dropbox/TB/LTBI/R/LTBIscreeningproject/ext-data/runs_1.1/18_to_35_in_2009/policy_003/cohort.RData")
UN_iso_country_codes <- read.csv("C:/Users/ngreen1/Dropbox/TB/LTBI/R/LTBIscreeningproject/data/UN_iso_country_codes.csv")

UN_iso_country_codes <- merge(UN_iso_country_codes, country_long_lat, by.x = "Country.or.Area", by.y = "name")

cohort_sizes <- as.data.frame(table(cohort$iso_n3_country))
cohort_sizes <- merge(cohort_sizes, UN_iso_country_codes, by.x = "Var1", by.y = "M49.code")

mapBubbles(cohort_sizes,
           nameX = 'longitude',
           nameY = 'latitude',
           nameZSize = 'Freq',
           nameZColour = 'black',
           fill = FALSE,
           addLegend = FALSE,
           add = TRUE,
           symbolSize = 1.5,
           lwdSymbols = 3)
