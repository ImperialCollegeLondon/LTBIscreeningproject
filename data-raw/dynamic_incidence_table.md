---
title: "LTBI screening model:
Dynamic incidence table"

author: "N Green"
date: "2018-10-31"
output:
  html_document:
    keep_md: TRUE
---


```r
#1. Vandenbroucke, J. P. & Pearce, N. Incidence rates in dynamic populations. Int. J. Epidemiol. 41, 1472–1479 (2012).


library(LTBIscreeningproject)
```

```
## Warning: replacing previous import 'crayon::reset' by 'git2r::reset' when
## loading 'LTBIscreeningproject'
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(reshape2)
library(tibble)

load(here::here("data", "model_input_cohort.RData"))

attach(IMPUTED_sample)

event_times <-
  list(
    tb = all_tb_issdt,
    # tb = notif_issdt.years,
    exit_uk = date_exit_uk1_issdt.years,
    death = date_death1_issdt.years)

detach(IMPUTED_sample)

lapply(event_times, head, 20)
```

```
## $tb
##  [1] Inf Inf Inf Inf Inf Inf Inf Inf Inf Inf Inf Inf Inf Inf Inf Inf Inf
## [18] Inf Inf Inf
## 
## $exit_uk
##  [1] 1.6075569       Inf       Inf 0.5805046       Inf 1.2794747       Inf
##  [8]       Inf       Inf 1.0064650 1.2184452       Inf       Inf       Inf
## [15] 0.4101696       Inf       Inf 7.9054158       Inf 8.3716472
## 
## $death
##  [1] 84.02946 70.83572 69.71176  1.33474 61.21660 53.16513 65.86426
##  [8] 81.14311 65.90251 43.69316 74.55214 43.56472 59.01388 62.63655
## [15] 77.21618 83.34842 47.86535 69.38264 67.97599 87.31882
```

```r
strat_pop_year <-
  event_times %>%
  count_comprsk_events() #%>%
  #as.tibble()

head(strat_pop_year)
```

```
##   year  tb exit_uk death fup total_tb total_exit_uk total_death total_fup
## 1    1 109       0     0   0      109             0           0         0
## 2    2 457       0     0   0      566             0           0         0
## 3    3 834       0     0   0     1400             0           0         0
## 4    4 832       0     0   0     2232             0           0         0
## 5    5 877       0     0   0     3109             0           0         0
## 6    6 670       0     0   0     3779             0           0         0
##   atrisk_end atrisk_start mean_atrisk incid_rate
## 1     403036       403145    403090.5   27.04107
## 2     402579       403036    402807.5  113.45370
## 3     401745       402579    402162.0  207.37912
## 4     400913       401745    401329.0  207.31121
## 5     400036       400913    400474.5  218.99022
## 6     399366       400036    399701.0  167.62530
```

```r
plot(strat_pop_year$tb, type = 'o',
     xlab = "Years since migration", ylab = "Cases of TB",
     xlim = c(0, 80), ylim = c(0, 1000))
```

![](dynamic_incidence_table_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
plot(strat_pop_year$incid_rate, type = 'o',
     xlab = "Years since migration", ylab = "Cases of TB per 100,000 person-years",
     xlim = c(0, 100), ylim = c(0, 300))
```

![](dynamic_incidence_table_files/figure-html/unnamed-chunk-1-2.png)<!-- -->


---
title: "dynamic_incidence_table.R"
author: "ngreen1"
date: "Wed Oct 31 13:32:17 2018"
---
