
# original dataset
#
# tb_years <- as.numeric(format(IMPUTED_IOM_ETS_WHO_merged_15_2_9$rNotificationDate, '%Y'))
#
# issdt_years <- as.numeric(format(IMPUTED_IOM_ETS_WHO_merged_15_2_9$issdt, '%Y'))
#
# fup_years <-
#   as.Date(IMPUTED_IOM_ETS_WHO_merged_15_2_9$fup1, origin = "1960-01-01") %>%
#   format('%Y') %>%
#   as.numeric()


# after preprocessing dataset

tb_years <- as.numeric(format(IMPUTED_sample$rNotificationDate, '%Y'))

issdt_years <- as.numeric(format(IMPUTED_sample$issdt, '%Y'))

fup_years <-
  as.Date(IMPUTED_sample$fup1, origin = "1960-01-01") %>%
  format('%Y') %>%
  as.numeric()


tab <- merge(table(fup_years - issdt_years) %>% as.data.frame(),
             table(tb_years - issdt_years) %>% as.data.frame(),
             by = "Var1") %>%
  setNames(nm = c("years", "fup", "tb")) %>%
  dplyr::filter(years != -1) %>%
  dplyr::mutate(cum_fup = cumsum(fup),
                remain = max(cum_fup) - cum_fup,
                atrisk = lag(remain, default = 0),
                atrisk = ifelse(atrisk == 0, max(cum_fup), atrisk),
                haz = tb/atrisk)
                # mean_remain = remain + (remain - lag(remain, default = 1))/2)

tab$mean_atrisk <- tab$atrisk + diff(c(tab$atrisk, 0))/2
tab$haz2 <- with(tab, tb/mean_atrisk)

plot(1:nrow(tab) - 1, tab$haz * 100000)
plot(1:nrow(tab) - 1, tab$haz2 * 100000)

