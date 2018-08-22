context("test-nmb_matrix.R")


ce1 <- list(e = data.frame("1" = c(2,0),
                           "2" = c(2,2),
                           check.names = FALSE),
            c = data.frame("1" = c(2,2),
                           "2" = c(2,0),
                           check.names = FALSE))

ce0 <- list(e = data.frame("1" = c(1,0),
                           "2" = c(1,1),
                           check.names = FALSE),
            c = data.frame("1" = c(1,1),
                           "2" = c(1,0),
                           check.names = FALSE))
nmb_mat <-
  nmb_matrix(ce1,
             ce0,
             design_mat = data.frame(scenario = c(1,2),
                                     Agree_to_Screen_cost = c(50,25),
                                     Agree_to_Screen_p = c(0.5,1)),
             wtp_min = 0,
             wtp_max = 10000)

test_that("same inputs", {

  expect_is(nmb_mat, "list")

  expect_equal(nmb_mat$`10000`$NMB[1], 1 * 10000 - 1)
  expect_equal(nmb_mat$`10000`$NMB[3], 2 * 10000 - 2)

  expect_equal(nmb_mat$`0`$NMB[1], -1)
  expect_equal(nmb_mat$`0`$NMB[3], -2)

  expect_equal(names(nmb_mat),
               c("0", "10000"))

  expect_equal(
    names(nmb_mat[[1]]),
    c("scenario",
      "Agree_to_Screen_cost",
      "Agree_to_Screen_p",
      "runs",
      "NMB",
      "type",
      "wtp"))

})
