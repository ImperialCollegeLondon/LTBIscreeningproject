# **************************************
# project: LTBI screening
# N Green
# Oct 2016
#


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

p_incid_year <- c(Lancet_prob,
                  p_incid_sutherland[max_years_obs:(interv$FUP_MAX_YEAR - max_years_obs)])
p_incid_year2 <- c(Lancet_prob,
                   suth2)
p_incid_year4 <- c(Lancet_prob,
                   suth4)
p_incid_year_ald <- c(Lancet_prob,
                   rep(0, interv$FUP_MAX_YEAR - max_years_obs)) # zeros after Aldridge


## comment out appropriately
# p_incid_year <- p_incid_year2
# p_incid_year <- p_incid_year4
# p_incid_year <- p_incid_year_ald

save(p_incid_year, file = "data/p_incid_year.RData")


#########
# plots #
#########

# plot(seq(0, length(Lancet_prob + 1)), c(0,Lancet_prob), type = 'o',
#      ylim = c(0,0.0025), xlim = c(0,100), lwd = 2,
#      xlab = "Time (years)", ylab = "Probability")
# lines(seq(0, length(p_incid_year2 + 1)), c(0,p_incid_year2), type = 'l', lty = 3, col = "blue", lwd = 2)
# lines(seq(0, length(p_incid_year4 + 1)), c(0,p_incid_year4), type = 'l', lty = 3, col = "red", lwd = 2)
# lines(seq(0, length(p_incid_year + 1)), c(0,p_incid_year), type = 'l', lty = 3, lwd = 2)
# legend('topright', legend = c("Aldridge", "Sutherland", "Sutherlandx2", "Sutherlandx4"),
#        col = c('black', 'black', 'blue', 'red'), lty = c(1,3,3,3), pch = c('o', NA,NA,NA), lwd = c(2,2,2,2), bty = 'n')
