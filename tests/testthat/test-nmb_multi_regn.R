context("test-nb_multi_regn.R")


ce1 <-
  list(
    e = data.frame(
      "1" = c(2, 2),
      "2" = c(2, 2),
      check.names = FALSE
    ),
    c = data.frame(
      "1" = c(2, 2),
      "2" = c(2, 2),
      check.names = FALSE
    ))

ce0 <-
  list(
    e = data.frame(
      "1" = c(1, 1),
      "2" = c(1, 1),
      check.names = FALSE
    ),
    c = data.frame(
      "1" = c(1, 1),
      "2" = c(1, 1),
      check.names = FALSE
    ))

nmb_mat <-
  nmb_matrix(ce1,
             ce0,
             design_mat = data.frame(scenario = c(1,2),
                                     Agree_to_Screen_cost = c(50,25),
                                     Agree_to_Screen_p = c(0.5,1)),
             wtp_min = 20000,
             wtp_max = 20000)

nmb_multi_regn(nmb_mat,
               interactions = "(Agree_to_Screen_cost * Agree_to_Screen_p)")

test_that(" ", {

  # expect_equal()
})
