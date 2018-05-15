library(QALY)

# EWNI
tb_tab <- table(IMPUTED_sample$rNotificationDate_issdt.years)
tb_tab <- prop.table(tb_tab[-length(tb_tab)])
tb_tab %*% discount(t_limit = length(tb_tab))

# all
tb_tab <- table(IMPUTED_sample$all_tb_issdt)
tb_tab <- prop.table(tb_tab[-length(tb_tab)])
tb_tab %*% discount(t_limit = length(tb_tab))

# exit
tb_tab <- table(IMPUTED_sample$exituk_tb.years)
tb_tab <- prop.table(tb_tab[-length(tb_tab)])
tb_tab %*% discount(t_limit = length(tb_tab))



# cohort QALY summary statistics
## TB
quantile(cohort$QALY_statusquo, probs = c(0.05,0.5,0.95), na.rm = T)
## disease-free
quantile(cohort$QALY_diseasefree, probs = c(0.05,0.5,0.95), na.rm = T)
