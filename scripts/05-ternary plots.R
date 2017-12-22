# *****************************
# LTBI screening
# N Green
# Dec 2017
#
# ternary plots


tern_data <- pred_INMB_30000
tern_data$startcomplete <- tern_data$Start/100 * tern_data$Complete/100
tern_data$startncomplete <- tern_data$Start/100 * (1 - tern_data$Complete/100)
tern_data$nstart <- 1 - tern_data$Start/100


# plot --------------------------------------------------------------------

# triangle averages

discarded <- TRUE

df <- dplyr::filter(tern_data, Agree == 50, Effective == 50)
df.prep <- prepareDF(data = df,
                     inc = 0.10,
                     discardZero = discarded)

df.prep$data$posneg <- df.prep$data$N
df.prep$data$posneg[df.prep$data$posneg < 0] <- Inf

s1 <-
  ggtern(data = df.prep$data, aes(x = L, y = T, z = R)) +
  theme_common() +
  geom_polygon(aes(fill = posneg, group = IDLabel)) +
  geom_text(data = subset(df.prep$labels, N != 0.0), aes(label = sprintf("%.1f", N), color = N), color = 'white', size = 2.0) +
  labs(title = "", fill = "INMB Value (Mean)") +
  ggtitle("Agree = 50 & Effective = 50") +
  guides(fill = FALSE)


df <- dplyr::filter(tern_data, Agree == 50, Effective == 100)
df.prep2 <- prepareDF(data = df,
                      inc = 0.10,
                      discardZero = discarded)

df.prep2$data$posneg <- df.prep2$data$N
df.prep2$data$posneg[df.prep2$data$posneg < 0] <- Inf


s2 <-
  ggtern(data = df.prep2$data, aes(x = L, y = T, z = R)) +
  theme_common() +
  geom_polygon(aes(fill = posneg, group = IDLabel)) +
  geom_text(data = subset(df.prep2$labels, N != 0.0), aes(label = sprintf("%.1f", N), color = N), color = 'white', size = 2.0) +
  labs(title = "", fill = "INMB (Mean)") +
  ggtitle("Agree = 50 & Effective = 100") +
  guides(fill = FALSE)


df <- dplyr::filter(tern_data, Agree == 100, Effective == 50)
df.prep2 <- prepareDF(data = df,
                      inc = 0.10,
                      discardZero = discarded)

df.prep2$data$posneg <- df.prep2$data$N
df.prep2$data$posneg[df.prep2$data$posneg < 0] <- Inf


s3 <-
  ggtern(data = df.prep2$data, aes(x = L, y = T, z = R)) +
  theme_common() +
  geom_polygon(aes(fill = posneg, group = IDLabel)) +
  geom_text(data = subset(df.prep2$labels, N != 0.0), aes(label = sprintf("%.1f", N), color = N), color = 'white', size = 2.0) +
  labs(title = "", fill = "INMB (Mean)") +
  ggtitle("Agree = 100 & Effective = 50") +
  guides(fill = FALSE)

df <- dplyr::filter(tern_data, Agree == 100, Effective == 100)
df.prep2 <- prepareDF(data = df,
                      inc = 0.10,
                      discardZero = discarded)

df.prep2$data$posneg <- df.prep2$data$N
df.prep2$data$posneg[df.prep2$data$posneg < 0] <- Inf


s4 <-
  ggtern(data = df.prep2$data, aes(x = L, y = T, z = R)) +
  theme_common() +
  geom_polygon(aes(fill = posneg, group = IDLabel)) +
  geom_text(data = subset(df.prep2$labels, N != 0.0), aes(label = sprintf("%.1f", N), color = N), color = 'white', size = 2.0) +
  labs(title = "", fill = "INMB (Mean)") +
  ggtitle("Agree = 100 & Effective = 100") +
  guides(fill = FALSE)

print(
  grid.arrange(arrangeGrob(s1, s3),
               arrangeGrob(s2, s4),
               ncol = 2)
)

# g <- arrangeGrob(s1, s2, s3, s4, nrow = 2)

