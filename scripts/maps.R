
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

png(filename = here::here("output", "incidence_map.png"),
    width = 40, height = 20, units = "cm", res = 72)

# mapDevice('x11')
mapParams <-
  mapCountryData(gtdMap,
                 nameColumnToPlot = 'e_inc_100k',
                 catMethod = 'fixedWidth',
                 # numCats = 100,
                 mapTitle = ""
  )

do.call(addMapLegend, c(mapParams,
                        legendLabels = "all",
                        # cutVector = 500,
                        legendWidth = 0.5))


# bubbles

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
           addLegend = TRUE,
           legendVals = c(1000, 5000, 10000),
           legendPos = "topleft",
          legendBg = NA,
           add = TRUE,
          legendTitle = "",
           symbolSize = 1.5,
           lwdSymbols = 3)

dev.off()


# ---


library(ggplot2)
library(maps)

mdat <- map_data('world')


str(mdat)

dat <- read.table(text="
ISO3V10   Country   'No of Documents'    Lat  Lon
ARG     Argentina   41          -64 -34
AUS     Australia   224         133 -27
CAN     Canada      426         -95 60
IRL     Ireland 68           -8 53
ITA     Italy             583           12.8333 42.8333
NLD     Netherlands 327          5.75   52.5
NZL     'New Zealand' 26           174    -41
ESP     Spain             325            -4  40
GBR     'United Kingdom'  2849             -2 54
USA     'United States'   3162            -97 38
", header=TRUE)

ggplot() +
  geom_polygon(dat=mdat, aes(long, lat, group=group), fill="grey50") +
  geom_point(data=dat,
             aes(x=Lat, y=Lon, map_id=Country, size=`No.of.Documents`), col="red")


