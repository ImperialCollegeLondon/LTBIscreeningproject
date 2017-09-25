#
# project: LTBI screening
# N Green
#
# fit full parametric model
# for extrapolation/prediction


##TODO: age dependent for death


# naive fit ---------------------------------------------------------------

# yearly
fs1 <- flexsurvreg(Surv(times_years, cens) ~ 1,
                   data = dat_surv_naive,
                   dist = "llogis")
                   # dist = "gamma")

plot(NA, type = 'n', xlim = c(0, 10), ylim = c(0.95, 1), ylab = "S(t) Kaplan-Meier")
lines(fs1)
plot(fs1, type = "hazard", ylim = c(0,0.01))
plot(fs1, type = "cumhaz")

# daily
##slow
# sp1 <- flexsurvspline(Surv(times, cens) ~ 1,
#                       data = dat_surv_naive, k = 3)
#
# plot(sp1, type = "cumhaz")
# plot(sp1, type = "hazard")
# abline(h = 1e-5, col = "blue")

# yearly
sp1 <- flexsurvspline(Surv(times_years, cens) ~ 1,
                      data = dat_surv_naive, k = 3, scale = "hazard")

plot(sp1, type = "cumhaz")
plot(sp1, type = "hazard", ylim = c(0,0.01))
abline(h = 0.004, col = "blue")


# imputed progression after follow-up -------------------------------------

sp1 <- flexsurvspline(Surv(times, cens) ~ 1,
                      data = dat_surv_imputed_uk_tb, k = 2)
plot(sp1, type = "cumhaz")
plot(sp1, type = "hazard")


# fit multistate models ---------------------------------------------------
# survivor functions

##slow
flex_impute <- flexsurvreg(Surv(time, status) ~ trans,
                           data = dat_surv_long,
                           # dist = "gengamma")
                           dist = "llogis")

plot(NULL, xlim = c(0, 10), ylim = c(0.7,1), type = "n", xlab = "", ylab = "S(t)")
lines(flex_impute)

plot(density(qllogis(p = seq(0,1,0.01), shape = 1.75640, scale = 44.15933)), main = "", xlim = c(0,100))


# non-joint distn fit
## quicker

flex_impute.list <- vector(3, mode = "list")

for (i in 1:3) {
  flex_impute.list[[i]] <-
    flexsurvreg(Surv(time, status) ~ 1,
                subset = (trans == i),
                data = dat_surv_long,
                dist = "llogis")
}

plot(NULL, xlim = c(0, 10), ylim = c(0.9, 1), type = "n", xlab = "", ylab = "S(t)")
lines(flex_impute.list[[1]], col = "red")  #tb
lines(flex_impute.list[[2]], col = "blue")  #exit uk
lines(flex_impute.list[[3]], col = "green") #death

plot(density(qllogis(p = seq(0,1,0.01), shape = 1.7869, scale = 42.4676)), main = "", xlim = c(0,100))


# cumulative transition-specific hazards ----------------------------------
# semi-parametric

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



# full parametric model
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
  dplyr::mutate(difference = Haz - lag(Haz))

ggplot(data = plot_dat,
       aes(x = time, y = difference, colour = trans, group = trans)) +
  geom_line()


# prediction transition probs -----------------------------------------------

pmatrix <- pmatrix.fs(x = flex_impute,
                      t = seq(0, 100, by = 0.5),
                      trans = tmat)
pmatrix <-
  lapply(pmatrix, t) %>%
  plyr::ldply(data.frame)

pmatrix$trans <- c(1,2,3,4)
pmatrix$'.id' <- as.numeric(pmatrix$'.id')


ggplot(data = pmatrix,
       aes(x = .id, y = X1, colour = trans, group = trans)) +
  geom_step() +
  ylim(0, 0.05) + ylab("cumulative trans probs")


# annual difference in probability
plot_dat <-
  pmatrix %>%
  dplyr::filter(trans == 1) %>%
  mutate(difference = X1 - lag(X1))

ggplot(data = plot_dat,
       aes(x = .id, y = difference, colour = trans, group = trans)) +
  geom_line()



# by simulation
sim_out <- pmatrix.simfs(x = flex_impute,
                         trans = tmat,
                         t = 20)



