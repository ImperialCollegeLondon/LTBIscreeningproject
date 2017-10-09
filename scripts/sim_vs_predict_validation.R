
# observed against predicted
# validation
# model sim vs regn predictions


# regression predictions
head(pred_data_30000)


# original model output
sim_INMB <-
  tidyr::spread(
    subset(sim_matrix, wtp == 30000), policy, NMB) %>%
  mutate(INMB = screened - statusquo) %>%
  arrange(Effective, Agree, Complete, Start)

validation_df <- merge(sim_INMB, pred_data_30000, by.x = TRUE)

plot(validation_df)
