# **********************************
# LTBI screening
# N Green
#
# data summary table


source("scripts/create_LTBI_input_workspace.R")
# load(file = "ext-data/LTBI_input_workspace.RData")

IMPUTED_sample_year_cohort <- dplyr::filter(IMPUTED_sample,
                                            issdt_year == year_cohort)

out <-
  cbind(
    "Description" = c("Observed non-TB cases in EWNI",
                      "Observed TB cases in EWNI",
                      "Imputed non-TB cases in EWNI",
                      "Imputed TB cases in EWNI",
                      "Imputed non-TB cases after exit",
                      "Imputed TB cases after exit",
                      "Imputed non-case-fatalities all TB",
                      "Imputed case-fatalities all TB"),
    rbind(

      cbind("Total" = table(IMPUTED_sample_year_cohort$uk_tb_orig),
            table(IMPUTED_sample_year_cohort$uk_tb_orig, IMPUTED_sample_year_cohort$who_prev_cat_Pareek2011)),

      cbind(table(IMPUTED_sample_year_cohort$uk_tb),
            table(IMPUTED_sample_year_cohort$uk_tb, IMPUTED_sample_year_cohort$who_prev_cat_Pareek2011)),

      cbind(table(IMPUTED_sample_year_cohort$exituk_tb),
            table(IMPUTED_sample_year_cohort$exituk_tb, IMPUTED_sample_year_cohort$who_prev_cat_Pareek2011)),

      cbind(table(IMPUTED_sample_year_cohort$tb_fatality),
            table(IMPUTED_sample_year_cohort$tb_fatality, IMPUTED_sample_year_cohort$who_prev_cat_Pareek2011)))#,
    # rbind(
    #
    #   table(IMPUTED_sample_year_cohort$uk_tb_orig, IMPUTED_sample_year_cohort$visatype2),
    #
    #   table(IMPUTED_sample_year_cohort$uk_tb, IMPUTED_sample_year_cohort$visatype2),
    #
    #   table(IMPUTED_sample_year_cohort$exituk_tb, IMPUTED_sample_year_cohort$visatype2),
    #
    #   table(IMPUTED_sample_year_cohort$tb_fatality, IMPUTED_sample_year_cohort$visatype2))
  )

rownames(out) <- NULL
out

write.csv(out, "output/data-summary-table.csv")
