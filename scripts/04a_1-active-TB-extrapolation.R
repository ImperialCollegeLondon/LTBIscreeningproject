# **************************************
# project: LTBI screening
# N Green
# Oct 2016
#


data("p_incid_sutherland")
data("incidence_Lancet")


Lancet_prob <- incidence_Lancet$mean/100000

max_years_obs <- length(Lancet_prob)

# append estimates to observed
# Lancet paper plot + Sutherland

p_incid_year <- c(Lancet_prob,
                  p_incid_sutherland[max_years_obs:(interv$FUP_MAX_YEAR - max_years_obs)])

save(p_incid_year, file = "data/p_incid_year.RData")

