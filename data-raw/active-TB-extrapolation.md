---
title: "LTBI screening model:
Extrapolate observed incidence curve"

author: "N Green"
date: "2018-09-26"
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
data("p_incid_sutherland")
data("incidence_Lancet")


# from per 100,000
Lancet_prob <- incidence_Lancet$mean/100000

max_years_obs <- length(Lancet_prob)

# append estimates to observed
# Lancet paper plot + Sutherland

suth <- p_incid_sutherland[max_years_obs:(interv$FUP_MAX_YEAR - max_years_obs)]

suth2 <- suth[seq(1,length(suth), by = 2)]
suth2 <- c(suth2, rep(tail(suth2,1), length(suth) - length(suth2) + 1))

suth4 <- suth[seq(1,length(suth), by = 4)]
suth4 <- c(suth4, rep(tail(suth4,1), length(suth) - length(suth4) + 1))

p_incid_year  <- c(Lancet_prob,
                   p_incid_sutherland[max_years_obs:(interv$FUP_MAX_YEAR - max_years_obs)])
p_incid_year2 <- c(Lancet_prob,
                   suth2)
p_incid_year4 <- c(Lancet_prob,
                   suth4)
p_incid_year_ald <- c(Lancet_prob,
                   rep(0, interv$FUP_MAX_YEAR - max_years_obs)) # zeros after Aldridge


## select
## comment out appropriately
# p_incid_year <- p_incid_year2
# p_incid_year <- p_incid_year4
# p_incid_year <- p_incid_year_ald

save(p_incid_year, file = here::here("data", "p_incid_year.RData"))


#########
# plots #
#########

plot(seq(0, length(Lancet_prob + 1)), c(0,Lancet_prob)*100000, type = 'o',
     ylim = c(0,0.0025)*100000, xlim = c(0,100), lwd = 2,
     xlab = "Years since migration", ylab = "Incidence per 100,000 person-years")
lines(seq(0, length(p_incid_year2 + 1)), c(0,p_incid_year2)*100000, type = 'l', lty = 3, col = "blue", lwd = 2)
lines(seq(0, length(p_incid_year4 + 1)), c(0,p_incid_year4)*100000, type = 'l', lty = 3, col = "red", lwd = 2)
lines(seq(0, length(p_incid_year + 1)), c(0,p_incid_year)*100000, type = 'l', lty = 3, lwd = 2)
legend('topright', legend = c("Aldridge", "Sutherland", "Sutherlandx2", "Sutherlandx4"),
       col = c('black', 'black', 'blue', 'red'), lty = c(1,3,3,3), pch = c('o', NA,NA,NA), lwd = c(2,2,2,2), bty = 'n')
```

![](04a_1-active-TB-extrapolation_files/figure-html/unnamed-chunk-1-1.png)<!-- -->


---
title: "04a_1-active-TB-extrapolation.R"
author: "ngreen1"
date: "Wed Sep 26 10:06:38 2018"
---
