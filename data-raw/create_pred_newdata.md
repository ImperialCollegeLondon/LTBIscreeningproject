---
output: 
  html_document: 
    keep_md: yes
---

#' ---
#' title: "LTBI screening model:
#' CE boundary plot for a range of effectiveness"
#'
#' author: "N Green"
#' date: "2018-11-12"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---



```r
library(LTBIscreeningproject)
```

```
## Warning: replacing previous import 'crayon::reset' by 'git2r::reset' when
## loading 'LTBIscreeningproject'
```


```r
dat <-
  create_pred_newdata(
  # sens = 0.9,
  # spec = 0.85,
  start = seq(0.5, 1, by = 0.02),
  complete = seq(0.5, 1, by = 0.02)
)
```