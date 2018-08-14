context("test-nmb_matrix.R")


ce1 <- list(e = data.frame("1" = c(2,2), "2" = c(2,2), check.names = FALSE),
            c = data.frame("1" = c(2,2), "2" = c(2,2), check.names = FALSE))

ce0 <- list(e = data.frame("1" = c(1,1), "2" = c(1,1), check.names = FALSE),
            c = data.frame("1" = c(1,1), "2" = c(1,1), check.names = FALSE))

nmb_mat <-
  nmb_matrix(ce1,
             ce0,
             design_matrix = data.frame(scenario = c(1,2),
                                        Agree_to_Screen_cost = c(50,25),
                                        Agree_to_Screen_p = c(0.5,1)),
             wtp_min = 20000,
             wtp_max = 22000)

test_that("same inputs", {

  expect_is(nmb_matrix, "list")

  expect_equal(nmb_matrix[[1]]$NMB[1:2],
               nmb_matrix[[1]]$NMB[3:4])

})
