data_test <- data.frame(LTBI = c(0,1,1,1),
                        exit_uk1 = c(T,F,F,F),
                        death1 = c(F,T,F,F),
                        uk_tb = c(F,F,T,F),
                        rNotificationDate_issdt.years = c(2,2,2,2),
                        fup_issdt = c(3,3,3,3),
                        date_death1_issdt.years = c(8,8,8,8))

sim_uktb_times(data = data_test,
               prob = year_prob.activetb_cmprsk_exituk/0.278)


#
# sim_uktb_times(data = IMPUTED_sample_year_cohort,
#                prob = year_prob.activetb_cmprsk_exituk/0.278)

x <- NULL
for (i in 1:1000) {

  x <- c(x,
         (sample_tb_year(fup_issdt = 5,
                         death_issdt = 60,
                         prob = year_prob.activetb_cmprsk_exituk/0.278)))
}


uktb.years <- sim_uktb_times(data = IMPUTED_sample_year_cohort,
                             prob = year_prob.activetb_cens_exituk/0.278)

table(floor(uktb.years))
table(floor(uktb.years)) %>% plot(ylim = c(0,150))

exit.years <- sim_exituk_tb_times(data = IMPUTED_sample_year_cohort,
                                  prob = year_prob.activetb_cens_exituk/0.278)

table(floor(exit.years))
table(floor(exit.years)) %>% plot(ylim = c(0,150))


#
# everyone LTBI has a chance of tb progression

data_test <- IMPUTED_sample_year_cohort
data_test$exit_uk1 = FALSE
data_test$death1 = FALSE
data_test$uk_tb = FALSE
data_test$date_death1_issdt.years = 100

uktb.years <- sim_uktb_times(data = data_test,
                             prob = year_prob.activetb_cens_exituk/0.278)

table(floor(uktb.years))
table(floor(uktb.years)) %>% plot(ylim = c(0,250), xlim = c(0,20))

# compare with year year_prob.activetb_cens_exituk
xx <- table(floor(uktb.years)) %>% as.data.frame()
plot(xx$Freq/nrow(data_test), xlim = c(0,20), ylim = c(0,0.0025), type = "h")

