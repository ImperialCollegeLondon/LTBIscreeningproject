context("test-nmb_scenarios.R")

test_that("basics", {

  e0 <- data.frame(a = c(1,0), b = c(2,1))
  e1 <- data.frame(a = c(1,0), b = c(2,1))
  c0 <- data.frame(a = c(1,0), b = c(2,2))
  c1 <- data.frame(a = c(1,0), b = c(2,2))
  wtp <- 10

  out <- nmb_scenarios(e0, c0,
                       e1, c1,
                       wtp)

  expect_is(out, "data.frame")
  expect_length(out, 5)
  expect_equal(nrow(out), 8)
  expect_equal(names(out), c("runs", "scenario", "NMB", "type", "wtp"))

  expect_equal(1*10 - 1, out$NMB[1])
  expect_equal(0*10 - 0, out$NMB[2])
  expect_equal(2*10 - 2, out$NMB[3])
  expect_equal(1*10 - 2, out$NMB[4])
  expect_equal(1*10 - 1, out$NMB[5])
  expect_equal(0*10 - 0, out$NMB[6])
  expect_equal(2*10 - 2, out$NMB[7])
  expect_equal(1*10 - 2, out$NMB[8])

  expect_equal(out$type,
               rep(c("statusquo", "screened"), each = 4))

})
