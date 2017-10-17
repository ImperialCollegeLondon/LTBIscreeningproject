#
# LTBI screening
# N Green
#
# observed against predicted
# validation
# model sim vs regn predictions


# regression predictions
head(pred_data_30000)
head(pred_NMB_30000)
head(pred_sim_30000_wide)


pred_sim_30000_long <- melt(data = pred_sim_30000_wide,
                            id.vars = c("Start", "Complete", "Agree", "Effective", "policy"),
                            variable_name = "pred_num")

sim_NMB_30000 <-
  sim_matrix %>%
  subset(wtp == 30000)

sim_NMB_30000$pred_num <- unique(pred_sim_30000_long$pred_num)


NMB_pred_and_sim <-
  merge(x = sim_NMB_30000, y = pred_NMB_30000,
        all.x = TRUE, all.y = FALSE,
        by = c("Start", "Complete", "Agree", "Effective", "policy"))

sim_meanNMB_30000 <-
  sim_NMB_30000 %>%
  group_by(scenario, Start, Complete, Agree, Effective, policy) %>%
  summarise(mean_NMB = mean(NMB)) %>%
  merge(y = pred_NMB_30000,
        all.x = TRUE, all.y = FALSE,
        by = c("Start", "Complete", "Agree", "Effective", "policy"))

# select required plot_data
NMB_predsim_and_sim <-
  merge(x = sim_NMB_30000, y = pred_sim_30000_long,
        all.x = FALSE, all.y = FALSE,
        by = c("Start", "Complete", "Agree", "Effective", "policy", "pred_num"))


# plots -------------------------------------------------------------------

sp <-
  subset(NMB_pred_and_sim, policy == "screened") %>%
  ggplot(aes(x = pred, y = NMB, color = scenario)) +
  geom_point() +
  scale_color_gradientn(colours = rainbow(5)) +
  ylim(12200, 12900) + xlim(12200, 12900) +
  geom_abline(slope = 1) +
  xlab("Metamodel expected NMB") +
  ylab("Original model NMB")
sp

sp <-
  subset(sim_meanNMB_3000, policy == "screened") %>%
  ggplot(aes(x = pred, y = mean_NMB, color = scenario)) +
  geom_point() +
  scale_color_gradientn(colours = rainbow(5)) +
  ylim(12200, 12900) + xlim(12200, 12900) +
  geom_abline(slope = 1) +
  xlab("Metamodel expected NMB") +
  ylab("Original model expected NMB")
sp


sp <-
  subset(NMB_predsim_and_sim, policy == "screened") %>%
  ggplot(aes(x = value, y = NMB, color = scenario)) +
  geom_point() +
  scale_color_gradientn(colours = rainbow(5)) +
  ylim(12200, 12900) + xlim(12200, 12900) +
  geom_abline(slope = 1) +
  xlab("Metamodel NMB") +
  ylab("Original model NMB")
sp

