
#' count_comprsk_events
#'
#' This replaces a (deprecated) previous rewritten version.
#'
#' @param event_times List; tb, exit_uk, death
#'
#' @return
#' @export
#'
#' @examples
#'
count_comprsk_events <- function(event_times) {

  times_dat <-
    lapply(event_times, ceiling) %>%
    data.frame()

  times_dat$id <- seq_len(nrow(times_dat))

  # find first event for each id
  first_times <-
    melt(times_dat, id.vars = "id") %>%
    group_by(id) %>%
    slice(which.min(value))

  # count number of events at each time
  count_times <-
    first_times %>%
    group_by(variable, value) %>%
    summarise(count = length(id)) %>%
    data.table()

  # rearrange to events as columns
  ##TODO: test data.table version
  res <-
    dcast.data.table(count_times, value ~ variable,
                     value.var = "count") %>%
    dplyr::rename(year = value)

  res[is.na(res)] <- 0

  mutate(res,
         total_tb = cumsum(tb),
         total_exit_uk = cumsum(exit_uk),
         total_death = cumsum(death),
         atrisk_end = nrow(times_dat) - (total_tb + total_exit_uk + total_death),
         atrisk_start = dplyr::lag(atrisk_end),
         atrisk_start = ifelse(is.na(atrisk_start), nrow(times_dat), atrisk_start),
         mean_atrisk = atrisk_end + (atrisk_start - atrisk_end)/2,
         incid_rate = tb/mean_atrisk*100000)
}
