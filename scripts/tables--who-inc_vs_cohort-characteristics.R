#
# tables of characteristic by who incidence
# screening target group
# for the total data set
#


library(sjPlot)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(reshape2)
library(LTBIscreeningproject)

data("model_input_cohort")


out <-
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

group <- dimnames(out)[[1]]
colyear <- dimnames(out)[[2]]
dimnames(out) <- NULL
out <- as.data.frame(out)
names(out) <- colyear
out$group <- group

out <- out %>% select(group, everything())
sjPlot::tab_df(out)




row_vars <- c("visatype2", "agegp2", "uk_tb", "all_tb", "LTBI", "screen")
col_var <- "who_inc_Pareek2011"
wide_df <- IMPUTED_sample[ , c(col_var, row_vars)]
wide_df$uk_tb <- as.factor(as.numeric(wide_df$uk_tb))
wide_df$all_tb <- as.factor(as.numeric(wide_df$all_tb))
wide_df$LTBI <- as.factor(wide_df$LTBI)
wide_df$screen <- as.factor(wide_df$screen)

output <- freq_table_for_publication(wide_df,
                                     row_vars,
                                     col_var)
sjPlot::tab_df(output, file = 'output/cohort_freq_table.doc')

