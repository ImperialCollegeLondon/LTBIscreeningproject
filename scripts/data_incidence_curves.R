#
# plot incidence curves from data
#
#


## using survival format data

xx <- table(times_years) %>%
  as.data.frame() %>%
  set_names(nm = c("times_years", "num_events"))

xx$num_remain <- length(cens) - xx$num_events
xx$cum_event <- cumsum(xx$num_events)
xx$cum_remain <- length(cens) - xx$cum_event

yy <- table(times_years = times_years[cens == 1]) %>%
  as.data.frame() %>%
  set_names(nm = c("times_years", "num_tb"))

yy <- merge(xx, yy, by = "times_years")
yy$atrisk <- c(length(cens), yy$cum_remain[-8])
yy$mean_atrisk <- yy$atrisk + diff(c(yy$atrisk, 0))/2
yy$haz <- yy$num_tb/yy$atrisk
yy$haz2 <- yy$num_tb/yy$mean_atrisk

yy

plot(yy$num_tb, type = "h", ylab = "number of cases")
plot(yy$haz*100000, type = "h", ylab = "haz")#, ylim = c(0, 200))
plot(yy$haz2*100000, type = "h", ylab = "haz")#, ylim = c(0, 200))


# direct from data set

## all dataset
# plot
par(mfcol = c(1, 2))
table(ceiling(IMPUTED_sample$rNotificationDate_issdt.years)) %>% plot(ylim = c(0,700))#, xlim = c(0,20))
table(ceiling(IMPUTED_sample$rNotificationDate_issdt.years[IMPUTED_sample$uk_tb_orig == "1"])) %>% lines(col = "red")#, xlim = c(0,20))
table(ceiling(IMPUTED_sample$exituk_tb.years)) %>% plot(ylim = c(0,700))#, xlim = c(0,20))

# table
xx <- merge(merge(as.data.frame(table(ceiling(IMPUTED_sample$rNotificationDate_issdt.years))),
            as.data.frame(table(ceiling(IMPUTED_sample$rNotificationDate_issdt.years[IMPUTED_sample$uk_tb_orig == "1"]))), by = "Var1", all = TRUE),
      as.data.frame(table(ceiling(IMPUTED_sample$exituk_tb.years))), by = "Var1")

xx$Var1 <- as.numeric(xx$Var1)
dataset_tb_times_all <- xx %>% arrange(Var1) %>% set_names(c("year", "EWNI_combined", "EWNI_obs", "exit"))

## 2009 cohort
# plots
par(mfcol = c(1, 2))
table(ceiling(IMPUTED_sample_year_cohort$rNotificationDate_issdt.years)) %>% plot(ylim = c(0,100))#, xlim = c(0,20))
table(ceiling(IMPUTED_sample_year_cohort$rNotificationDate_issdt.years[IMPUTED_sample_year_cohort$uk_tb_orig == "1"])) %>% lines(col = "red", xlim = c(0,20))
table(ceiling(IMPUTED_sample_year_cohort$exituk_tb.years)) %>% plot(ylim = c(0,200))#, xlim = c(0,20))

# table
xx <- merge(merge(as.data.frame(table(ceiling(IMPUTED_sample_year_cohort$rNotificationDate_issdt.years))),
                  as.data.frame(table(ceiling(IMPUTED_sample_year_cohort$rNotificationDate_issdt.years[IMPUTED_sample_year_cohort$uk_tb_orig == "1"]))), by = "Var1", all = TRUE),
            as.data.frame(table(ceiling(IMPUTED_sample_year_cohort$exituk_tb.years))), by = "Var1")

xx$Var1 <- as.numeric(xx$Var1)
dataset_tb_times_2009 <- xx %>% arrange(Var1) %>% set_names(c("year", "EWNI_combined", "EWNI_obs", "exit"))

write.csv(dataset_tb_times_all, file = "dataset_tb_times_all.csv")
write.csv(dataset_tb_times_2009, file = "dataset_tb_times_2009.csv")
