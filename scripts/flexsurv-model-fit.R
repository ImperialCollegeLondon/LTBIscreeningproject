#
# project: LTBI screening
# N Green
#
# fit full parametric model
# useful for extrapolation/prediction


##TODO: age dependent for death


# naive fit ---------------------------------------------------------------

fs1 <- flexsurvreg(Surv(times_years, cens) ~ 1,
                   data = dat_surv_naive,
                   dist = "llogis")

plot(NA, type = 'n', xlim = c(0, 10), ylim = c(0.95, 1))
lines(fs1)
plot(fs1, type = "hazard")
plot(fs1, type = "cumhaz")

sp1 <- flexsurvspline(Surv(times, cens) ~ 1,
                      data = dat_surv_naive, k = 3)

plot(sp1, type = "cumhaz")
plot(sp1, type = "hazard")
abline(h = 1e-5, col = "blue")

# yearly
sp1 <- flexsurvspline(Surv(times_years, cens) ~ 1,
                      data = dat_surv_naive, k = 3)

plot(sp1, type = "cumhaz")
plot(sp1, type = "hazard")
abline(h = 0.004, col = "blue")


# imputed progression after follow-up -------------------------------------

sp1 <- flexsurvspline(Surv(times, cens) ~ 1,
                      data = dat_surv_imputed_uk_tb, k = 2)
plot(sp1, type = "cumhaz")
plot(sp1, type = "hazard")


# fit multistate models ---------------------------------------------------
# survivor functions

flex_impute <- flexsurvreg(Surv(time, status) ~ trans,
                           data = dat_surv_long,
                           dist = "gengamma")

plot(NULL, xlim = c(0, 10), ylim = c(0.7,1), type = "n", xlab = "", ylab = "")
lines(flex_impute)


flex_impute.list <- vector(3, mode = "list")

# quicker non-joint fit
for (i in 1:3) {
  flex_impute.list[[i]] <-
    flexsurvreg(Surv(time, status) ~ 1,
                subset = (trans == i),
                data = dat_surv_long,
                dist = "llogis")
}

plot(NULL, xlim = c(0, 10), ylim = c(0.2, 1), type = "n", xlab = "", ylab = "")
lines(flex_impute.list[[1]])
lines(flex_impute.list[[2]], col = "blue")
lines(flex_impute.list[[3]], col = "green")


# cumulative transition-specific hazards ----------------------------------

tmat <- rbind(c(NA, 1, 2, 3),
              c(NA, NA, NA, NA),
              c(NA, NA, NA, NA),
              c(NA, NA, NA, NA))

# Cox model semi-parametric
crcox <- coxph(Surv(time, status) ~ strata(trans),
               data = dat_surv_long)

# piece-wise constant estimates
mrcox <- mstate::msfit(object = crcox,
                       trans = tmat)

plot(mrcox)

plot_dat <-
  mrcox$Haz %>%
  dplyr::filter(trans == 1) %>%
  mutate(difference = Haz - lag(Haz))

ggplot(data = plot_dat,
       aes(x = time, y = Haz, colour = trans, group = trans)) +
  geom_line()


# annual difference in cumulative hazards
plot_dat <-
  mrcox$Haz %>%
  dplyr::filter(trans == 1) %>%
  mutate(difference = Haz - lag(Haz))

ggplot(data = plot_dat,
       aes(x = time, y = difference, colour = trans, group = trans)) +
  geom_line()



# full parametric
tgrid <- seq(0, 20, 1)

mrwei <- msfit.flexsurvreg(object = flex_impute,
                           t = tgrid,
                           trans = tmat)

plot(mrwei)


ggplot(data = mrwei$Haz,
       aes(x = time, y = Haz, colour = trans, group = trans)) +
  geom_line()

# annual difference in cumulative hazards
plot_dat <-
  mrwei$Haz %>%
  dplyr::filter(trans == 1) %>%
  mutate(difference = Haz - lag(Haz))

ggplot(data = plot_dat,
       aes(x = time, y = difference, colour = trans, group = trans)) +
  geom_line()


# prediction transition probs -----------------------------------------------

pmatrix <- pmatrix.fs(x = flex_impute,
                      t = seq(0, 10, by = 0.5),
                      trans = tmat)
pmatrix <-
  lapply(pmatrix, t) %>%
  plyr::ldply(data.frame)

pmatrix$trans <- c(1,2,3,4)
pmatrix$'.id' <- as.numeric(pmatrix$'.id')


ggplot(data = pmatrix,
       aes(x = .id, y = X1, colour = trans, group = trans)) +
  geom_step() +
  ylim(0, 1)


# annual difference in probability
plot_dat <-
  pmatrix %>%
  dplyr::filter(trans == 2) %>%
  mutate(difference = X1 - lag(X1))

ggplot(data = plot_dat,
       aes(x = .id, y = difference, colour = trans, group = trans)) +
  geom_line()



# by simulation
pmatrix.simfs(x = flex_impute,
              trans = tmat,
              t = 20)



