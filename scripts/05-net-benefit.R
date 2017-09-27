# ************************************
# LTBI screening
# N Green
# Sept 2017
#
# net benefit regression (not incremental)



# basic model -------------------------------------------------------------

# frequentist
# lm_basic_wtp <- lm_wtp(as.formula(NMB ~ policy), design_matrix)

# bayesian
# lm_basic_wtp <- bayeslm_wtp(as.formula(NMB ~ policy), design_matrix)
#
# lm_basic <-
#   lapply(wtp_seq, lm_basic_wtp) %>%
#   set_names(wtp_seq)

# create cea curves -------------------------------------------------------

# beta_pos <- sapply(lm_basic,
#                    function(x) summary(x)$coefficients["policyscreened", "Estimate"] > 0)
#
# pvalue <- sapply(lm_basic,
#                  function(x) summary(x)$coefficients["policyscreened", "Pr(>|t|)"])  #2-sided
#
# ceac <- ifelse(beta_pos, 1 - pvalue/2, pvalue/2)  #https://bmchealthservres.biomedcentral.com/articles/10.1186/1472-6963-6-68
# plot(y = ceac, x = wtp_seq, type = "o")


# multiariate -----------------------------------------------

##interactions
nmb_formula <- as.formula(NMB ~
                            policy*Agree*Start +
                            policy*Agree*Complete +
                            policy*Agree*Effective +
                            policy*Start*Complete +
                            policy*Start*Effective +
                            policy*Complete*Effective)

# expanded formula
# terms = attr(terms.formula(nmb_formula), "term.labels")
# f = as.formula(sprintf("y ~ %s", paste(terms, collapse="+")))


# lm_multi_wtp <- lm_wtp(nmb_formula, design_matrix)
lm_multi_wtp <- bayeslm_wtp(nmb_formula, design_matrix)

lm_multi <-
  lapply(wtp_seq, lm_multi_wtp) %>%
  purrr::set_names(wtp_seq)



# note: covariates are bounded [0,100]
# is this a problem?

# centred for easier coefficient interpretation

# one variable
# ------------

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


opt_thesholds_agree <- optimal_thresholds(lm_multi_agree, covar = "Agree", centre = 50)
opt_thesholds_start <- optimal_thresholds(lm_multi_start, "Start", centre = 50)
opt_thesholds_complete <- optimal_thresholds(lm_multi_complete, "Complete", centre = 50)
opt_thesholds_effective <- optimal_thresholds(lm_multi_effective, "Effective", centre = 50)


png(paste(plots_folder_scenario, "optimal_thesholds.png", sep = "/"))

plot(NA, type = 'n', xlim = c(min(wtp_seq) ,max(wtp_seq)), ylim = c(0, 100), xlab = "Willingness to pay (£)", ylab = "Probability")
lines(wtp_seq, opt_thesholds_agree, type = 'l', col = "red")
lines(wtp_seq, opt_thesholds_start, type = 'l', col = "blue")
lines(wtp_seq, opt_thesholds_complete, type = 'l', col = "green")
lines(wtp_seq, opt_thesholds_effective, type = 'l', col = "black")

dev.off()


##TODO: two variable threshold plots
#
# sapply(lm_multi,
#        function(x) centre - (x$coefficients["policyscreened"] + x$coefficients["x"]*x)/x$coefficients[sprintf("policyscreened:I(%s - %s)", covar, centre)])
#
# solve()


##TODO: regn coeff plots
#




# create cea curves -------------------------------------------------------

##TODO: adapt for extended model
# predict(lm_multi_complete[[41]], type = )

# beta_pos <- sapply(lm_multi_complete,
#                    function(x) summary(x)$coefficients["policyscreened", "Estimate"] > 0)
#
# pvalue <- sapply(lm_basic,
#                  function(x) summary(x)$coefficients["policyscreened", "Pr(>|t|)"])  #2-sided
#
# ceac <- ifelse(beta_pos, 1 - pvalue/2, pvalue/2)  #https://bmchealthservres.biomedcentral.com/articles/10.1186/1472-6963-6-68
# plot(y = ceac, x = wtp_seq, type = "o")


# output tables -----------------------------------------------------------

library(broom)
library(stargazer)

# INMB estimate
# lapply(lm_basic, function(x) tidy(x)[2, ]) %>%
#   do.call(rbind, .)

lm_multi_all_agree <-
  lapply(lm_multi_agree, function(x) tidy(x)) %>%
  plyr::join_all(by = "term") %>%
  rbind(c("wtp", rep(wtp_seq, each = 4)))

lm_multi_all_start <-
  lapply(lm_multi_start, function(x) tidy(x)) %>%
  plyr::join_all(by = "term")

lm_multi_all_complete <-
  lapply(lm_multi_complete, function(x) tidy(x)) %>%
  plyr::join_all(by = "term")

lm_multi_all_effective <-
  lapply(lm_multi_effective, function(x) tidy(x)) %>%
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


# save --------------------------------------------------------------------

write.csv(x = lm_multi_all_agree,
          file = paste(diroutput, "lm_table_agree.csv", sep = "/"))

write.csv(x = lm_multi_all_start,
          file = paste(diroutput, "lm_table_start.csv", sep = "/"))

write.csv(x = lm_multi_all_complete,
          file = paste(diroutput, "lm_table_complete.csv", sep = "/"))

write.csv(x = lm_multi_all_effective,
          file = paste(diroutput, "lm_table_effective.csv", sep = "/"))

