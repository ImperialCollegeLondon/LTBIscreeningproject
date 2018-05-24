#
# stacked bar chart and violin plots
# for the total data set
#
# - country by visa status
# - who incidence category by time in EWNI
# - age group incidence category by time in EWNI
# - country by time in EWNI
# - visa status by time in EWNI


library(ggplot2)
library(reshape2)
library(dplyr)
library(LTBIscreeningproject)

data("model_input_cohort")

dat <- unclass(table(IMPUTED_sample$country,
                     IMPUTED_sample$visatype2)) %>% data.frame()

dat$country <- rownames(dat)
dat2 <- melt(dat, id.vars = "country")

ggplot(dat2, aes(x = variable, y = value, fill = country)) +
  geom_bar(stat = "identity") +
  xlab("\nvisa category") +
  ylab("count\n") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dat <- data.frame(exit_issdt = IMPUTED_sample$date_exit_uk1_issdt.years,
                  incid_in_orgin = IMPUTED_sample$who_inc_Pareek2011,
                  country = IMPUTED_sample$country,
                  visa = IMPUTED_sample$visatype2,
                  agegp2 = IMPUTED_sample$agegp2)

ggplot(dat, aes(x = incid_in_orgin, y = exit_issdt)) +
  # geom_boxplot()
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

ggplot(dat, aes(x = agegp2, y = exit_issdt)) +
  # geom_boxplot()
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

ggplot(dat, aes(x = country, y = exit_issdt)) +
  # geom_boxplot()
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dat, aes(x = visa, y = exit_issdt)) +
  # geom_boxplot()
  # geom_violin()
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# geom_violin(scale = "count")


rbind(
  table(IMPUTED_sample$visatype2,
        ceiling(IMPUTED_sample$date_exit_uk1_issdt.years)),
  prop.table(table(IMPUTED_sample$visatype2,
                   ceiling(IMPUTED_sample$date_exit_uk1_issdt.years)), margin = 1) %>% round(2),
  table(IMPUTED_sample$agegp2,
        ceiling(IMPUTED_sample$date_exit_uk1_issdt.years)),
  prop.table(table(IMPUTED_sample$agegp2,
                   ceiling(IMPUTED_sample$date_exit_uk1_issdt.years)), margin = 1) %>% round(2),
  table(IMPUTED_sample$who_inc_Pareek2011,
        ceiling(IMPUTED_sample$date_exit_uk1_issdt.years)),
  prop.table(table(IMPUTED_sample$who_inc_Pareek2011,
                   ceiling(IMPUTED_sample$date_exit_uk1_issdt.years)), margin = 1) %>% round(2))
