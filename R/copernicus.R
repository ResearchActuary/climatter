#' download_one_file
#' 
#' Make a single request to Copernicus. 
#' 
#' @param user_name The user name for the service
#' @param date_in The date associated with the data
#' @param variables_in String vector of values to retrieve
#' @param save_path Path where the data should be saved. The default is a folder called "data"
#' 
#' @return The function will return the name of the downloaded file, as a string
#' 
#' @details 
#' 
#' @importFrom ecmwfr wf_request
#' @importFrom lubridate day
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @export
download_one_file <- function(user_name, date_in, variables_in, save_path = "data") {
  
  times = sprintf("%02i", 1:24 - 1)
  times = paste0(times, ":00")
  
  my_request <- list(
    dataset_short_name = "reanalysis-era5-land"
    , variable = variables_in
    , year = lubridate::year(date_in)
    , month = lubridate::month(date_in)
    , day = lubridate::day(date_in)
    , time = times
    , area = c(90, -172, 24, -52)
    , format = "netcdf"
    , target = paste(date_in, "era5.nc", sep = "-")
  )
  
  wf_request(
    user = user_name
    , request = my_request   
    , transfer = TRUE  
    , path = save_path
    , verbose = FALSE
  )
}

#' extract_one_file
#' 
#' @details This does little apart from converting the contents of an NC file into a data frame. Note that this object will likely be very large, on the order of more than one million rows. 
#' 
#' If a column called 'time' exists in the data frame, it will be renamed 'cds_time'. This column name is expected in the function `add_date_time()`.
#' 
#' @param file_in The name of an NC file from which to extract
#' @param delete_source Boolean indicating whether to delete the file once the extract has completed
#' 
#' @return A tibble (data frame) of the contents
#' 
#' @importFrom tidync tidync
#' @importFrom tidync hyper_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr rename
#' 
#' @export
extract_one_file <- function(file_in, delete_source = TRUE) {
  
  tbl_out <- file_in %>% 
    tidync() %>% 
    hyper_tibble()
  
  if ('time' %in% names(tbl_out)) {
    tbl_out <- tbl_out %>% 
      rename(cds_time = time)
  }
  
  if (delete_source) {
    unlink(file_in)
  }
  
  tbl_out
}

