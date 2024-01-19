library(readwritesqlite)
library(tidyhydat)
library(tidyverse)
library(cli)
library(DBI)


# see a list of parameters available
param_id <- param_id

# get alist of stations with realtime data
stations <- realtime_stations(prov_terr_state_loc = 'BC') %>%
  pull(STATION_NUMBER)

# get list of all params that contain discharge from param_id using stringr and dplyr::filter
discharge_params <- param_id %>%
  filter(str_detect(Name_En, "Discharge")) %>%
  pull(Parameter)


# get air temp, flow, prcip incremental and accumulated precipitation and any discharge for any sites that have water temper
realtime_watertemp <- function(id_station = NULL,
                     param_primary = c(5),
                     param_secondary = c(1, 46, 18, 19, discharge_params),
                     days_back = 5400){
  tryCatch(
    {
      primary_data <- tidyhydat::realtime_ws(
        station_number = id_station,
        parameters = param_primary,
        start_date = Sys.Date() - days_back,
        end_date = Sys.Date()
      )

      if (!is.null(primary_data)) {
        secondary_data <- tryCatch(
          {
            tidyhydat::realtime_ws(
              station_number = id_station,
              parameters = param_secondary,
              start_date = Sys.Date() - days_back,
              end_date = Sys.Date()
            )
          },
          error = function(e) {
            cli::cli_alert(paste("Failed to retrieve secondary data for station", id_station, "Error:", e$message))
            return(NULL)
          }
        )
      }

      list(primary_data = primary_data, secondary_data = secondary_data)
    },
    error = function(e) {
      cli::cli_alert(paste("Failed to retrieve primary data for station", id_station, "Error:", e$message))
      return(NULL)
    }
  )
}

# get data for all stations
dat_raw <- stations %>%
  purrr::map(realtime_watertemp, days_back = 10)


dat <- dat_raw %>%
  discard(is.null) %>%
  purrr::map(bind_rows) %>%
  bind_rows()


# create a new  sqlite database  and write dat to it
# DBI::dbConnect(RSQLite::SQLite(), paste0(dir, "/data/realtime_data.sqlite"))
# conn <- rws_connect(paste0(dir, "/data/realtime_data.sqlite"))
# rws_write(dat, exists = F, delete = F,
#           conn = conn, x_name = "realtime_data")
# rws_disconnect(conn)






