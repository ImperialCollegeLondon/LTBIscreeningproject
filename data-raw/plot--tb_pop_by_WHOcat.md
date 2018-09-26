---
title: "LTBI screening model:
histograms and survival curves stratifying by WHO incidence in cob"

author: "N Green"
date: "2018-09-25"
output:
  html_document:
    keep_md: TRUE
---


```r
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
## select subgroup to plot
SCREENED <- c(0,1)
# SCREENED <- 1

# subset by covariate?
cohort1 <-
  IMPUTED_sample %>%
  dplyr::filter(who_inc_Pareek2011 == "(150,250]")#,
                # screen %in% SCREENED)
cohort2 <-
  IMPUTED_sample %>%
  dplyr::filter(who_inc_Pareek2011 == "(250,350]")#,
                # screen %in% SCREENED)
cohort3 <-
  IMPUTED_sample %>%
  dplyr::filter(who_inc_Pareek2011 == "(350,1e+05]")#,
                # screen %in% SCREENED)


# histograms with different bin sizes -------------------------------------

hist(cohort1$notif_issdt.years[!is.na(cohort1$notif_issdt.years)],
     breaks = c(0,1,2,3,4,5,6,7,8,9,15,50,100),
     col = "black",
     prob = T,
     main = "",
     xlab = "year")
hist(cohort2$notif_issdt.years[!is.na(cohort2$notif_issdt.years)],
     breaks = c(0,1,2,3,4,5,6,7,8,9,15,50,100),
     prob = TRUE,
     add = T,
     col = "blue",
     border = "blue")
hist(cohort3$notif_issdt.years[!is.na(cohort3$notif_issdt.years)],
     breaks = c(0,1,2,3,4,5,6,7,8,9,15,50,100),
     prob = TRUE,
     add = T,
     col = "white",
     border = "red")
```

![](plot--tb_pop_by_WHOcat_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
# density plots -----------------------------------------------------------

plot(density(cohort1$notif_issdt.years, from = 0, bw = 1, na.rm = TRUE),
     lwd = 2,
     ylim = c(0, 0.004),
     main = "")
lines(density(cohort2$notif_issdt.years, from = 0, bw = 1, na.rm = TRUE),
      lwd = 2, col = "blue")
lines(density(cohort3$notif_issdt.years, from = 0, bw = 1, na.rm = TRUE),
      lwd = 2, col = "red")

legend('topright',
       legend = c("(150,250]","(250,350]","(350,1e+05]"),
       col = c("black","blue","red"),
       lty = 1)


# survival plots ----------------------------------------------------------

library(survminer)
```

```
## Loading required package: ggplot2
```

```
## Loading required package: ggpubr
```

```
## Loading required package: magrittr
```

```r
library(survival)

# include all-cause death in risk set
IMPUTED_sample$notif_issdt.years[is.na(IMPUTED_sample$notif_issdt.years)] <- Inf

fit_tb <- survfit(Surv(notif_issdt.years) ~ who_inc_Pareek2011,
                  # data = IMPUTED_sample[IMPUTED_sample$screen == 1, ])
                  data = IMPUTED_sample)

ggsurvplot(fit_tb,
           data = IMPUTED_sample,
           risk.table = TRUE,
           ylim = c(0.95, 1),
           # xlim = c(0, 20),
           conf.int = TRUE,
           main = "TB notification in EWNI")#,
```

```
## Warning: Removed 4 rows containing missing values (geom_path).
```

```
## Warning: Removed 4 rows containing missing values (geom_path).
```

![](plot--tb_pop_by_WHOcat_files/figure-html/unnamed-chunk-1-2.png)<!-- -->![](plot--tb_pop_by_WHOcat_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
# ggtheme = theme_minimal(),
# risk.table.y.text.col = T,
# risk.table.y.text = FALSE)

# fit_exit <- survfit(Surv(IMPUTED_sample$date_exit_uk1_issdt.years) ~ who_inc_Pareek2011,
#                     data = IMPUTED_sample)
#
# ggsurvplot(fit_exit,
#            data = IMPUTED_sample,
#            risk.table = TRUE,
#            main = "Exit EWNI")
```


---
title: "plot--tb_pop_by_WHOcat.R"
author: "ngreen1"
date: "Tue Sep 25 12:25:26 2018"
---
