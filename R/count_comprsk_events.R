
#' count_comprsk_events
#'
#' Counts competing risk events over time.
#' Replaces a (deprecated) previous rewritten version.
#' Tied times are prioritised according to their named order.
#'
#' @param event_times List; tb, (fup), exit_uk, death
#'
#' @return data.frame
#' @export
#' @import data.table
#'
#' @examples
#'
count_comprsk_events <- function(event_times) {

  times_dat <-
    lapply(event_times, ceiling) %>%
    data.frame()

  times_dat <- add_missing_cols(times_dat, 'fup')

  times_dat <- prioritise_events(times_dat)

  times_dat$id <- seq_len(nrow(times_dat))

  # find first event for each id
  ## dplyr
  # first_times <-
  #   melt(times_dat, id.vars = "id") %>%
  #   group_by(id) %>%
  #   slice(which.min(value))

  DT <-
    times_dat %>%
    melt(id.vars = "id") %>%
    data.table(key = c("variable", "id"))

  # find first event for each id
  DT_min <-
    DT[, min(value, na.rm = TRUE), by = id] %>%
    `colnames<-`(c("id", "value"))

  # join with original
  first_times <- DT_min[DT, on = .(id, value), nomatch = 0]

  # count number of events at each time
  count_times <-
    first_times %>%
    group_by(variable, value) %>%
    summarise(count = length(id)) %>%
    data.table()

  # rearrange to events as columns
  events_cols <-
    dcast.data.table(count_times, value ~ variable,
                     value.var = "count") %>%
    dplyr::rename(year = value) %>%
    as.data.frame()

  events_cols[is.na(events_cols)] <- 0
  events_cols <- add_missing_cols(events_cols, 'fup')

  mutate(events_cols,
         total_tb = cumsum(tb),
         total_exit_uk = cumsum(exit_uk),
         total_death = cumsum(death),
         total_fup = ifelse(is.na(fup), 0, cumsum(fup)),
         atrisk_end = nrow(times_dat) - (total_tb + total_fup + total_exit_uk + total_death),
         atrisk_start = dplyr::lag(atrisk_end),
         atrisk_start = ifelse(is.na(atrisk_start), nrow(times_dat), atrisk_start),
         mean_atrisk = atrisk_end + (atrisk_start - atrisk_end)/2,
         incid_rate = tb/mean_atrisk*100000)
}


#
add_missing_cols <- function(data,
                             colname) {

  add <- colname[!colname %in% names(data)]

  if (length(add) != 0) data[add] <- NA
  data
}


# replace same-year events with NA
# make sure that tb is always counted as priority
prioritise_events <- function(times_dat) {

  times_dat %>%
  mutate(
    tb = ifelse(is.infinite(tb), NA, tb),
    exit_uk = ifelse(is.infinite(exit_uk), NA, exit_uk),
    fup = ifelse(!is.na(tb), NA, fup),
    exit_uk = ifelse(!is.na(tb), NA, exit_uk),
    death = ifelse(!is.na(tb), NA, death),

    exit_uk = ifelse(!is.na(fup), NA, exit_uk),
    death = ifelse(!is.na(fup), NA, death),
    death = ifelse(!is.na(exit_uk), NA, death))
}
