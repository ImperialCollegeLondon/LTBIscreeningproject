
#' count_comprsk_events
#'
#' Counts competing risk events over time.
#' Replaces a (deprecated) previous rewritten version.
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

  times_dat <- fncols(times_dat, 'fup')

  # make sure that tb is always counted as priority
  times_dat <-
    times_dat %>%
    mutate(
      fup = ifelse(!is.na(tb), NA, fup),
      exit_uk = ifelse(!is.na(tb), NA, exit_uk),
      death = ifelse(!is.na(tb), NA, death)) %>%
    mutate(
      exit_uk = ifelse(!is.na(fup), NA, exit_uk),
      death = ifelse(!is.na(fup), NA, death))

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
  res <-
    dcast.data.table(count_times, value ~ variable,
                     value.var = "count") %>%
    dplyr::rename(year = value)

  res[is.na(res)] <- 0


  mutate(res,
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


# add missing columns
fncols <- function(data,
                   cname) {

  add <- cname[!cname %in% names(data)]

  if (length(add) != 0) data[add] <- NA
  data
}
