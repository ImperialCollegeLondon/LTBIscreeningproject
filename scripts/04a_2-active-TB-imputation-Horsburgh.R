#
# project: LTBI screening
# N Green
#
#


age <- 18
annual_risk <- 0.0056 #subset(annual_risk_reactivation,
                      #age_grp == cut(age, breaks = c(0,5,15,35,55,200)),
                      #select = 'annual_risk') %>% unlist()
decades <- ceiling((100 - age)/10)
annual_risk <- annual_risk * 0.9^rep(1:decades, each = 10)
plot(annual_risk)

annual_H <- cumsum(annual_risk)
annual_f <- annual_risk*exp(-annual_H)
