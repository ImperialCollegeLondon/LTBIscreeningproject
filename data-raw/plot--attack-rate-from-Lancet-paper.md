---
title: "LTBI screening model:
use data from plot in Robs Lancet paper"

author: "N Green"
date: "2018-09-25"
output:
  html_document:
    keep_md: TRUE
---


```r
#

library(readr)
library(magrittr)
library(ggplot2)

incidence_mean <- read_csv("C:/Users/ngreen1/Dropbox/TB/LTBI/data/incidence-by-time-since-entry-mean.csv", col_names = FALSE)
```

```
## Parsed with column specification:
## cols(
##   X1 = col_double(),
##   X2 = col_double()
## )
```

```r
incidence_lower95 <- read_csv("C:/Users/ngreen1/Dropbox/TB/LTBI/data/incidence-by-time-since-entry-lower95.csv", col_names = FALSE)
```

```
## Parsed with column specification:
## cols(
##   X1 = col_double(),
##   X2 = col_double()
## )
```

```r
incidence_upper95 <- read_csv("C:/Users/ngreen1/Dropbox/TB/LTBI/data/incidence-by-time-since-entry-upper95.csv", col_names = FALSE)
```

```
## Parsed with column specification:
## cols(
##   X1 = col_double(),
##   X2 = col_double()
## )
```

```r
incidence_mean$X1 %<>% round()
incidence_lower95$X1 %<>% round()
incidence_upper95$X1 %<>% round()

incidence_Lancet <-
  plyr::join_all(list(incidence_mean,
                      incidence_lower95,
                      incidence_upper95), by = "X1") %>%
  set_names(nm = c("year", "mean", "lower95", "upper95"))

incid_long <- reshape2::melt(incidence_Lancet, id.vars = "year")

ggplot(data = incid_long, aes(x = year, y = value, col = variable)) + geom_line()
```

![](plot--attack-rate-from-Lancet-paper_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
# save(incidence_Lancet, file = "data/incidence_Lancet.RData")
```


---
title: "plot--attack-rate-from-Lancet-paper.R"
author: "ngreen1"
date: "Tue Sep 25 12:34:27 2018"
---
