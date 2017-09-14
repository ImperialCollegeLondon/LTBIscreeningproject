# ************************************
# LTBI screening
# N Green
# Sept 2017
#
# net benefit regression (not incremental)


net_benefit <- function(e_list, c_list, wtp_threshold) {

  mapply(FUN = function(e, c, wtp) (e * wtp) - c,
         e = e_list,
         c = c_list,
         MoreArgs = list(wtp = wtp_threshold),
         SIMPLIFY = FALSE)
}

# create long array over multiple wtp
nmb <- function(e_statusquo, c_statusquo,
                e_screened, c_screened,
                wtp_threshold) {

  list(statusquo = net_benefit(e_list = e_statusquo,
                               c_list = c_statusquo,
                               wtp_threshold),
       screened = net_benefit(e_list = e_screened,
                              c_list = c_screened,
                              wtp_threshold)) %>%
    reshape2::melt() %>%
    set_names(c("NMB", "scenario", "policy")) %>%
    mutate(wtp = wtp_threshold)
}

# partial linear regression function
lm_wtp <- function(nmb_formula,
                   design_matrix, ...) {
  function(threshold){

    lm(nmb_formula,
       data = subset(design_matrix, wtp == threshold))
  }
}

# partial bayesian linear regression function
bayeslm_wtp <- function(nmb_formula,
                        design_matrix, ...) {
  function(threshold){
    arm::bayesglm(formula = nmb_formula,
                  family = gaussian,
                  data = subset(design_matrix, wtp == threshold),
                  prior.mean = 0, prior.scale = Inf, prior.df = Inf)
  }
}


# create design matrix ----------------------------------------------------

# convert to percentages
scenario_numbers <- scenario_parameter_p$scenario
design_matrix <-
  (scenario_parameter_p*100) %>%
  mutate(scenario = scenario_numbers)

# convert to discrete levels
# design_matrix <- apply(scenario_parameter_p, 2, as.factor)

# conmbine decision tree and pop model output
e_screened <- map2(aTB_CE_stats$QALY.screened_person,
                   map(dectree_res, "mc_health"), `-`)

c_screened <- map2(aTB_CE_stats$cost.screened_person,
                   map(dectree_res, "mc_cost"), `+`)

e_statusquo <- aTB_CE_stats$QALY.statusquo_person
c_statusquo <- aTB_CE_stats$cost.statusquo_person

wtp_seq <- seq(10000, 20000, by = 250)

nmb_long <-
  lapply(wtp_seq,
         FUN = function(wtp) nmb(e_statusquo, c_statusquo,
                                 e_screened, c_screened,
                                 wtp)) %>%
  do.call(what = rbind, args = .)

design_matrix <- merge(x = design_matrix,
                       y = nmb_long,
                       by = "scenario")

# simplify names
names(design_matrix) <- gsub(pattern =  " Treatment", replacement = "", names(design_matrix))
names(design_matrix) <- gsub(pattern =  " to Screen", replacement = "", names(design_matrix))

# set baseline
design_matrix <- within(design_matrix,
                        policy <- factor(policy, levels = c("statusquo", "screened")))


# basic model -------------------------------------------------------------

# frequentist
# lm_basic_wtp <- lm_wtp(as.formula(NMB ~ policy), design_matrix)
# bayesian
lm_basic_wtp <- bayeslm_wtp(as.formula(NMB ~ policy), design_matrix)

lm_basic <-
  lapply(wtp_seq, lm_basic_wtp) %>%
  set_names(wtp_seq)


# create cea curves

beta_pos <- sapply(lm_basic,
                   function(x) summary(x)$coefficients["policyscreened", "Estimate"] > 0)

pvalue <- sapply(lm_basic,
                 function(x) summary(x)$coefficients["policyscreened", "Pr(>|t|)"])  #2-sided

ceac <- ifelse(beta_pos, 1 - pvalue/2, pvalue/2)  #https://bmchealthservres.biomedcentral.com/articles/10.1186/1472-6963-6-68
plot(y = ceac, x = wtp_seq, type = "o")


# multiariate -----------------------------------------------

nmb_formula <- as.formula(NMB ~ policy*Agree*Start +
                            policy*Agree*Complete +
                            policy*Agree*Effective +
                            policy*Start*Complete +
                            policy*Start*Effective +
                            policy*Complete*Effective)

# lm_multi_wtp <- lm_wtp(nmb_formula, design_matrix)
lm_multi_wtp <- bayeslm_wtp(nmb_formula, design_matrix)
lm_multi <- lapply(wtp_seq, lm_multi_wtp)



# note: covariates are bounded [0,100]
# is this a big problem?

# centred for easier coefficient interpretation

# one variable
nmb_formula <- as.formula(NMB ~ policy*I(Agree - 50))
lm_multi_wtp <- bayeslm_wtp(nmb_formula, design_matrix)
lm_multi_agree <- lapply(wtp_seq, lm_multi_wtp)

nmb_formula <- as.formula(NMB ~ policy*I(Start - 50))
lm_multi_wtp <- bayeslm_wtp(nmb_formula, design_matrix)
lm_multi_start <- lapply(wtp_seq, lm_multi_wtp)

nmb_formula <- as.formula(NMB ~ policy*I(Complete - 50))
lm_multi_wtp <- bayeslm_wtp(nmb_formula, design_matrix)
lm_multi_complete <- lapply(wtp_seq, lm_multi_wtp)

nmb_formula <- as.formula(NMB ~ policy*I(Effective - 50))
lm_multi_wtp <- bayeslm_wtp(nmb_formula, design_matrix)
lm_multi_effective <- lapply(wtp_seq, lm_multi_wtp)


optimal_thresholds <- function(lm_multi, covar, centre) {

  opt <-
    sapply(lm_multi,
           function(x) centre - x$coefficients["policyscreened"]/x$coefficients[sprintf("policyscreened:I(%s - %s)", covar, centre)])

  # remove out-of-bounds
  opt <-
    opt %>%
    set_names(wtp_seq) %>%
    ifelse(test = . < 100 & . > 0,
           yes =  .,
           no =  NA)

  return(opt)
}

opt_thesholds_agree <- optimal_thresholds(lm_multi_agree, covar = "Agree", centre = 50)
opt_thesholds_start <- optimal_thresholds(lm_multi_start, "Start", centre = 50)
opt_thesholds_complete <- optimal_thresholds(lm_multi_complete, "Complete", centre = 50)
opt_thesholds_effective <- optimal_thresholds(lm_multi_effective, "Effective", centre = 50)

plot(wtp_seq, opt_thesholds_agree, type = 'l', ylim = c(0, 100), col = "red")
lines(wtp_seq, opt_thesholds_start, type = 'l', ylim = c(0, 100), col = "blue")
lines(wtp_seq, opt_thesholds_complete, type = 'l', ylim = c(0, 100), col = "green")
lines(wtp_seq, opt_thesholds_effective, type = 'l', ylim = c(0, 100), col = "black")


# output tables -----------------------------------------------------------

library(broom)
library(stargazer)

# INMB estimate
# lapply(lm_basic, function(x) tidy(x)[2, ]) %>%
#   do.call(rbind, .)

lm_multi_all <-
  lapply(lm_multi, function(x) tidy(x)) %>%
  plyr::join_all(by = "term")

lm_multi_all <-
  lapply(lm_multi_agree, function(x) tidy(x)) %>%
  plyr::join_all(by = "term")

##TODO:
# doesnt like bayesglm
# stargazer(lm_basic,
#           type = "text",
#           column.labels = as.character(wtp_seq))
#
# stargazer(lm_multi,
#           type = "text",
#           column.labels = as.character(wtp_seq))
#
# cat(paste(xx, collapse = "\n"), "\n", file = "output/lm_table.txt", append = TRUE)

write.csv(x = lm_multi_all,
          file = paste(diroutput, "lm_table.csv", sep = "/"))

