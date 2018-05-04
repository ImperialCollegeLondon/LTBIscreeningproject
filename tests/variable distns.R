table(IMPUTED_sample$LTBI_or_activeTB, IMPUTED_sample$who_inc_Pareek2011) %>% prop.table(margin = 2)

# (0,50]  (50,150] (150,250] (250,350] (350,1e+05]
# FALSE        0.8683032 0.7985021 0.6978383   0.6971831
# TRUE         0.1316968 0.2014979 0.3021617   0.3028169
table(IMPUTED_sample$all_tb, IMPUTED_sample$who_inc_Pareek2011) %>% prop.table(margin = 2)

# (0,50]    (50,150]   (150,250]   (250,350] (350,1e+05]
# FALSE        0.994803110 0.992294397 0.988484213 0.988341105
# TRUE         0.005196890 0.007705603 0.011515787 0.011658895
table(IMPUTED_sample$tb_fatality, IMPUTED_sample$who_inc_Pareek2011, useNA = "always") %>% prop.table(margin = 2)

# (0,50]      (50,150]     (150,250]     (250,350]   (350,1e+05] <NA>
#   FALSE        0.00511238803 0.00750756157 0.01105725141 0.01119435567
# TRUE         0.00008450228 0.00019804119 0.00045853531 0.00046453956
# <NA>         0.99480310968 0.99229439723 0.98848421328 0.98834110477
table(IMPUTED_sample$agegroup_all_notification, IMPUTED_sample$who_inc_Pareek2011) %>% prop.table(margin = 2)

# (0,50]  (50,150] (150,250] (250,350] (350,1e+05]
# [15,45)         0.6422764 0.6051402 0.6257110   0.6464949
# [45,65)         0.2439024 0.2897196 0.2491468   0.2420611
# [65,200)        0.1138211 0.1051402 0.1251422   0.1114440
#

xx <- table(ceiling(IMPUTED_sample$date_death1_issdt.years), IMPUTED_sample$who_inc_Pareek2011) %>% prop.table(margin = 2)

plot(xx[ ,"(50,150]"], type = "l")
lines(xx[ ,"(150,250]"], col = "green")
lines(xx[ ,"(250,350]"], col = "red")
lines(xx[ ,"(350,1e+05]"], col = "blue")

xx <- table(ceiling(IMPUTED_sample$all_death_rNotificationDate), IMPUTED_sample$who_inc_Pareek2011, useNA = "always") %>% prop.table(margin = 2)

plot(xx[ -1,"(50,150]"], type = "l", xlim = c(0,80), ylim = c(0,0.001))
lines(xx[ ,"(150,250]"], col = "green")
lines(xx[ ,"(250,350]"], col = "red")
lines(xx[ ,"(350,1e+05]"], col = "blue")

xx <- table(ceiling(IMPUTED_sample$rNotificationDate_issdt.years), IMPUTED_sample$who_inc_Pareek2011) %>% prop.table(margin = 2)

plot(xx[ -nrow(xx),"(50,150]"], type = "l", xlim = c(0,80), ylim = c(0,0.001))
lines(xx[ ,"(150,250]"], col = "green")
lines(xx[ ,"(250,350]"], col = "red")
lines(xx[ ,"(350,1e+05]"], col = "blue")

xx <- table(round(IMPUTED_sample$QALY_diseasefree), IMPUTED_sample$who_inc_Pareek2011) %>% prop.table(margin = 2)
plot(xx[ ,"(50,150]"], type = "l")
lines(xx[ ,"(150,250]"], col = "green")
lines(xx[ ,"(250,350]"], col = "red")
lines(xx[ ,"(350,1e+05]"], col = "blue")

table(round(IMPUTED_sample$QALY_fatality), IMPUTED_sample$who_inc_Pareek2011, useNA = "always") %>% prop.table(margin = 2)

xx <- table(round(IMPUTED_sample$QALY_cured), IMPUTED_sample$who_inc_Pareek2011) %>% prop.table(margin = 2)
plot(xx[ ,"(50,150]"], type = "l")
lines(xx[ ,"(150,250]"], col = "green")
lines(xx[ ,"(250,350]"], col = "red")
lines(xx[ ,"(350,1e+05]"], col = "blue")
