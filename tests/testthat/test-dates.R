context("test-dates.R")

test_that("find first event date", {

  # load("C:/Users/ngreen1/Dropbox/TB/LTBI/R/LTBIscreeningproject/data/model_input_cohort.RData")
  #
  # date_origin <- as.Date("1960-01-01") #from STATA
  # FUP_DATE <- as.Date("2013-12-31") #aldridge
  #
  # fup1 <- as.Date(IMPUTED_sample$fup1,
  #                 origin = date_origin)
  #
  # event_dates <- data.frame(IMPUTED_sample[ ,c("date_death1", "date_exit_uk1")], FUP_DATE)
  #
  # event_dates$first_event <- apply(event_dates2, 1,
  #                                  function(x) x[min(which(x == min(x)))])
  # event_dates$fup1 <- fup1
  # event_dates$date_entry <- IMPUTED_sample$issdt
  # event_dates$date_tb <- IMPUTED_sample$rNotificationDate
  #
  # event_dates$first_event <- ifelse(is.na(event_dates$date_tb), event_dates$first_event, event_dates$date_tb)
  #
  # event_dates$diff <-
  #   difftime(as.Date(as.character(event_dates$first_event)),
  #            as.Date(as.character(fup1)),
  #            units = "days")
#
#   expect_true(sum(event_dates$diff == 0))

})
