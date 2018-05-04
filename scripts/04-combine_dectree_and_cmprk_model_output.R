# ******************************************************
# LTBI screening
# N Green
# Sept 2017
#
# combine decision tree and competing risk model output


screen_discount <- screen_discount(cohort)

# create BCEA dataframe ---------------------------------------------------

tb_cost <- from_list_to_BCEA(aTB_CE_stats$cost_incur_person)
tb_QALYgain <- from_list_to_BCEA(aTB_CE_stats$QALYgain_person)
LTBI_cost <- from_list_to_BCEA(purrr::map(dectree_res, "mc_cost"), screen_discount)
LTBI_QALYgain <- from_list_to_BCEA(purrr::map(dectree_res, "mc_health"), -screen_discount)

c.total <- as.matrix(LTBI_cost + tb_cost)
e.total <- as.matrix(LTBI_QALYgain + tb_QALYgain)

save(e.total, c.total,
     file = pastef(diroutput, "e_and_c_totals.RData"))


# create nmb matrix ----------------------------------------------------

# discounting due to delay to screening
dectree_res_mc_health <-
  lapply(purrr::map(dectree_res, "mc_health"), `*`, screen_discount)

dectree_res_mc_cost <-
  lapply(purrr::map(dectree_res, "mc_cost"), `*`, screen_discount)

# combine decision tree and pop model output
e_screened <- purrr::map2(aTB_CE_stats$QALY.screened_person,
                          dectree_res_mc_health, `-`)

c_screened <- purrr::map2(aTB_CE_stats$cost.screened_person,
                          dectree_res_mc_cost, `+`)

e_statusquo <- aTB_CE_stats$QALY.statusquo_person
c_statusquo <- aTB_CE_stats$cost.statusquo_person

wtp_seq <- seq(10000, 30000, by = 500)

# net monetary benefit by wtp
nmb_long <-
  lapply(wtp_seq,
         FUN = function(wtp) nmb(e_statusquo, c_statusquo,
                                 e_screened, c_screened,
                                 wtp)) %>%
  do.call(what = rbind, args = .)

sim_matrix <-
  merge(x = design_matrix,
        y = nmb_long,
        by = "scenario") %>%
  mutate(policy = factor(policy, levels = c("statusquo", "screened")))

save(sim_matrix, file = "data/sim_matrix.RData")


