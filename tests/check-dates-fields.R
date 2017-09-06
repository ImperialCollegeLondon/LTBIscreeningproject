
fup1 <- as.Date(IMPUTED_sample_year_cohort$fup1, origin = date_origin)

event_dates <- cbind(IMPUTED_sample_year_cohort[ ,c("date_death1", "date_exit_uk1")], FUP_DATE, fup1)

first_event_date <- apply(data.frame(IMPUTED_sample_year_cohort[ ,c("date_death1", "date_exit_uk1")], FUP_DATE), 1,
                          function(x) x[min(which(x==min(x)))])

event_dates$first_event <- first_event_date


difftime(as.Date(as.character(event_dates$first_event[12])),
         as.Date(as.character(event_dates$fup1[12])),
         units = "days")

