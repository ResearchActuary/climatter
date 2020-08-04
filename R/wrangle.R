#' trim_grid
#' 
#' @details 
#' 
#' 
#' @param tbl_in Data frame of climate data
#' @param lat_gran Granularity of latitude
#' @param lon_gran Granularity of longitude
#' 
#' @importFrom magrittr %>% 
#' @importFrom dplyr filter
#' 
#' @export
trim_grid <- function(tbl_in, lat_gran = .05, lon_gran = .05) {
  tbl_in %>% 
    filter(
      abs(longitude - round(longitude)) < lat_gran
      , abs(latitude - round(latitude)) < lon_gran
    )
}

cds_to_datetime <- function(x){
  
  x %>% 
    magrittr::multiply_by(60) %>%
    magrittr::multiply_by(60) %>%
    as.POSIXct(origin = "1900-01-01 00:00:00")
}

#' add_date_time
#' 
#' @importFrom dplyr mutate
#'
#' @export
add_date_time <- function(tbl_in) {
  
  tbl_in %>% 
    mutate(
      date_time = cds_to_datetime(cds_time)
      , date = as.Date(date_time)
    )
}

#' summarize_day
#' 
#' @param tbl_in Data frame of climate data to summarize
#' 
#' @return A data frame
#' 
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise_all
#' @importFrom dplyr ungroup
#' @importFrom magrittr %>% 
#' 
#' @export
summarize_day <- function(tbl_in) {
  
  keep_cols <- setdiff(names(tbl_in), c('date_time', 'cds_time'))
  
  tbl_in <- tbl_in[, keep_cols]
  
  tbl_in %>% 
    group_by(longitude, latitude, date) %>% 
    summarise_all(
      .funs = list(mean = mean, median = median, min = min, max = max)
      , na.rm = TRUE
    ) %>% 
    ungroup()
  
}

create_tracker_table <- function(months = 1:12, days = 1:31, years = 1981:2020) {
  
  tbl_requests <- crossing(
    month = months
    , day = days
    , year = years
  ) %>% 
    mutate(
      date = lubridate::make_date(year, month, day)
    ) %>% 
    filter(
      !is.na(date)
    ) %>% 
    select(-year, -month, -day)
  
}
