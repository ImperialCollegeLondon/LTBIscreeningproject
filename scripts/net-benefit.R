# ************************************
# LTBI screening
# N Green
#
# net benefit (not incremental)


net_benefit <- function(e_list, c_list) {

  mapply(FUN = function(e, c, wtp) (e * wtp) - c,
         e = e_list,
         c = c_list,
         MoreArgs = list(wtp = wtp_threshold),
         SIMPLIFY = FALSE)
}

e_screened <- map2(aTB_CE_stats$QALY.screened_person,
                   map(dectree_res, "mc_health"), `-`)

c_screened <- map2(aTB_CE_stats$cost.screened_person,
                   map(dectree_res, "mc_cost"), `+`)

net_monetary_benefit <- list(statusquo = net_benefit(e_list = aTB_CE_stats$QALY.statusquo_person,
                                                     c_list = aTB_CE_stats$cost.statusquo_person),
                             screened = net_benefit(e_list = e_screened,
                                                    c_list = c_screened))
net_monetary_benefit_long <-
  reshape2::melt(net_monetary_benefit) %>%
  set_names(c("NMB", "scenario", "policy"))

design_matrix <- merge(x = params,
                       y = net_monetary_benefit_long,
                       by = "scenario")

##TODO:
# do this for each scenario

# basic model
lm(NMB ~ screen)

# dichotomous covariates
lm(NMB ~ screen*agree*start +
          screen*agree*complete +
          screen*agree*effective +
          screen*start*complete +
          screen*start*effective +
          screen*complete*effective)

# continuous covariates
# lm(NMB ~ screen*agree*start +
#      screen*agree*complete +
#      screen*agree*effective +
#      screen*start*complete +
#      screen*start*effective +
#      screen*complete*effective)

# repeat for different wtp
#
# get ceac from regression results
