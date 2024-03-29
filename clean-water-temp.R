source("header.R")

sbf_set_sub("read", "water-temp")
sbf_load_datas()

water_temp %<>%
  mutate(
    site = basename(file),
    site = str_sub(site, end = -5L),
    site = str_to_title(site),
    flag = as_factor(Flag)
  ) %>%
  rename(
    temp = `Temperature (C)`,
    date_time = `DateTime (UTC)`
  ) %>%
  mutate(date_time = dtt_adjust_tz(date_time, tz = tz_analysis)) %>% 
  select(site, date_time, temp, flag)

colnames(water_temp_meta_data) <- "meta"

water_temp_meta_data %<>%
  mutate(
    meta_row = str_detect(meta, "Metadata"),
    site_name_row = lead(meta_row),
    site_visit_row = str_detect(meta, "^Site visit"),
    note_row = if_else2(
      !meta_row & !site_name_row & !site_visit_row,
      TRUE,
      FALSE
    ),
    site = if_else2(site_name_row, meta, NA_character_),
    site = str_sub(site, end = -2L)
  ) %>%
  fill(site, .direction = "down") %>%
  separate_wider_delim(
    site, 
    delim = " (", 
    names = c("site_description", "site"),
    cols_remove = TRUE
  ) %>%
  mutate(
    site = str_extract(site, "[[:alpha:]]{3}"),
    site_description = str_to_title(site_description),
    meta = if_else2(
      site == "MTL" & meta_row, 
      "Metadata: (54° 52' 45.4\" N, 125° 08' 32.4\" W; 716 m; 7/17/2020 - 6/29/2021)",
      meta
    )
  ) %>%
  select(
    site, site_description, meta, meta_row, site_name_row, site_visit_row, 
    note_row
  )

water_temp_site <-
  water_temp_meta_data %>%
  filter(meta_row) %>%
  select(site, site_description, meta) %>%
  mutate(
    meta = str_sub(meta, start = 12L, end = -2L)
  ) %>%
  separate_wider_delim(
    meta, 
    delim = "; ", 
    names = c("lat_long", "elev", "dates_in_water"),
    too_few = "align_start"
  ) %>%
  separate_wider_delim(
    dates_in_water,
    delim = " - ",
    names = c("start_date", "end_date")
  ) %>%
  separate_wider_delim(
    lat_long,
    delim = " N, ",
    names = c("lat", "long")
  ) %>% 
  mutate(
    Latitude = arc_to_dd(lat),
    Longitude = arc_to_dd(long),
    Longitude = -1 * Longitude,
    elev = str_sub(elev, end = -3L),
    elev = as.integer(elev),
    elev = set_units(elev, "m"),
    across(c(start_date, end_date), \(x) mdy(x))
  ) %>% 
  select(
    site, site_description, elev, Latitude, Longitude, start_date, end_date
  )

sites_long_lat <- water_temp_site %>% 
  select(site, Latitude, Longitude)

sites <- 
  water_temp_site %>%
  ps_longlat_to_sfc() %>%
  ps_sfcs_to_crs(crs = crs) %>% 
  select(site, geometry)

# Write shapefile to calculate SSN object
# st_write(sites, "output/objects/ssn/site.shp", append = FALSE)

water_temp_site %<>%
  ps_longlat_to_sfc()

water_temp_visit <-
  water_temp_meta_data %>%
  filter(site_visit_row) %>%
  select(site, meta) %>%
  mutate(
    date_visit = str_extract(meta, "(?<=Site visit on )\\d{2}-\\d{2}-\\d{4}"),
    date_visit = mdy(date_visit),
    date_logger_removed = case_when(
      str_detect(meta, "ogger removed at") ~ date_visit,
      str_detect(meta, "battery died on") ~ 
        mdy(str_extract(meta, "(?<=battery died on )\\d{2}-\\d{2}-\\d{4}")),
      .default = NA_Date_
    ),
    date_logger_returned = date_visit,
    date_logger_returned = if_else2(
      site == "CNE" & date_logger_removed == "2020-08-25",
      ymd("2020-08-26"),
      date_logger_returned
    ),
    time_logger_removed = case_when(
      str_detect(meta, "ogger removed at") ~ 
        dtt_time(str_c(str_extract(meta, "(?<=ogger removed at )\\d{2}:\\d{2}"), ":00")),
      str_detect(meta, "battery died on") ~ 
        dtt_time(str_c(str_extract(meta, "(?<=at )\\d{2}:\\d{2}"), ":00")),
      .default = NA
    ),
    time_logger_returned = case_when(
      str_detect(meta, "ogger removed at") ~ 
        dtt_time(str_c(str_extract(meta, "(?<=returned at )\\d{2}:\\d{2}"), ":00")),
      str_detect(meta, "battery died on") ~ 
        # Assumes battery back in order by end of visit day UTC time (no replacement time given)
        dtt_time("23:59:00"),
      .default = NA
    ),
    date_time_logger_removed = dtt_date_time(
      date_logger_removed, 
      time_logger_removed, 
      tz = tz_data
    ),
    date_time_logger_returned = dtt_date_time(
      date_logger_returned, 
      time_logger_returned,
      tz = tz_data
    ),
    across(starts_with("date_time"), \(x) dtt_adjust_tz(x, tz = tz_analysis)),
    date_visit = dtt_date(date_time_logger_removed)
  ) %>% 
  select(site, date_visit, date_time_logger_removed, date_time_logger_returned) %>% 
  pivot_longer(
    cols = starts_with("date_time"),
    names_to = "action",
    values_to = "date_time"
  ) %>% 
  group_by(site, date_visit) %>% 
  complete(date_time = seq(min(date_time), max(date_time), by = "min")) %>% 
  ungroup() %>% 
  mutate(data_downloading = TRUE) %>% 
  select(site, date_time, data_downloading)

water_temp_notes <- 
  water_temp_meta_data %>%
  filter(note_row) %>%
  group_by(site) %>%
  summarize(comment = paste(meta, collapse = " "), .groups = "keep") %>%
  ungroup() %>%
  mutate(
    dates_problem = str_extract_all(comment, "\\d+-\\d+-\\d+"),
    times_problem = str_extract_all(comment, "\\d{2}:\\d{2}")
  ) %>%
  unnest_wider(dates_problem, names_sep = "_") %>%
  unnest_wider(times_problem, names_sep = "_") %>%
  mutate(
    # Add dates for unclear notes from readme metadata .txt file.
    dates_problem_1 = case_when(
      site == "Rhine Creek Above Sweeney Creek (RSC)" ~ "07-21-2021",
      site == "Stuart River Above Nechako River (SNR)" ~ "06-08-2021",
      .default = dates_problem_1
    ),
    across(starts_with("dates_problem"), \(x) mdy(x)),
    across(starts_with("times_problem"), \(x) dtt_time(str_c(x, ":00"))),
    across(starts_with("times_problem"), \(x) if_else2(is.na(x), dtt_time(00:00:00), x)),
    date_time_start_problem = dtt_date_time(dates_problem_1, times_problem_1, tz = tz_data),
    date_time_end_problem = dtt_date_time(dates_problem_2, times_problem_2, tz = tz_data),
    date_time_end_problem = if_else2(site == "NSL", date_time_start_problem, date_time_end_problem),
    date_time_start_problem = if_else2(site == "NSL", NA, date_time_start_problem),
    across(starts_with("date_time"), \(x) dtt_adjust_tz(x, tz = tz_analysis))
  ) %>%
  select(site, date_time_start_problem, date_time_end_problem, comment)

### ^ Manually checked the problematic sites to see if flag == 'F' during the specified period: none had the flag all the way through the suspected problematic periods.

water_temp_problem_periods <- 
  water_temp_notes %>% 
  rename(
    start = date_time_start_problem,
    end = date_time_end_problem
  ) %>% 
  pivot_longer(
    cols = c(start, end),
    names_to = "boundary",
    values_to = "date_time"
  ) %>% 
  group_by(site) %>% 
  group_split() %>% 
  map(
    .x = .,
    .f = \(x) {
      if (is.na(x$date_time[x$boundary == "start"]) & 
          is.na(x$date_time[x$boundary == "end"])) {
        x %>% complete(
          date_time = seq(
            dtt_date_time(
              water_temp_site$start_date[water_temp_site$site == x$site[1]], 
              dtt_time("00:00:00"),
              tz = tz_analysis
            ),
            dtt_date_time(
              water_temp_site$end_date[water_temp_site$site == x$site[1]], 
              dtt_time("00:00:00"),
              tz = tz_analysis
            ),
            by = "5 mins"
          )
        ) %>% 
          fill(site, comment, .direction = "up") %>% 
          drop_na(date_time)
      } else if (is.na(x$date_time[x$boundary == "start"]) & 
                 !is.na(x$date_time[x$boundary == "end"])) {
        x %>% 
          complete(
            date_time = seq(
              dtt_date_time(
                water_temp_site$start_date[water_temp_site$site == x$site[1]],  
                dtt_time("00:00:00"),
                tz = tz_analysis
              ),
              max(date_time, na.rm = TRUE),
              by = "5 mins"
            )
          ) %>% 
          fill(site, comment, .direction = "up") %>% 
          drop_na(date_time)
      } else if (!is.na(x$date_time[x$boundary == "start"]) & 
                 !is.na(x$date_time[x$boundary == "end"])) {
        x %>% 
          complete(date_time = seq(min(date_time), max(date_time), by = "5 mins")) %>%
          fill(site, comment)
      }
    }
  ) %>% 
  bind_rows() %>% 
  mutate(problem = TRUE) %>% 
  select(site, date_time, problem, comment)

water_temp_flag <- tribble(
  ~flag,     ~flag_description,
  "P",                  "Pass",
  "F",                  "Fail",
  "B",  "Backwater conditions",
  "D",      "Data downloading",
  "I",     "Issue with logger"
)

sbf_set_sub("clean", "water-temp")
sbf_save_datas()

if(FALSE) {
  sbf_compare_data_archive()
}