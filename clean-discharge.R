source("header.R")

sbf_set_sub("read", "discharge")
sbf_load_datas()

runoff %<>% 
  mutate(date = as_date(time, origin = origin)) %>% 
  select(
    runoff = RUNOFF,
    longitude = lon,
    latitude = lat,
    date
  )

baseflow %<>% 
  mutate(date = as_date(time, origin = origin)) %>% 
  select(
    baseflow = BASEFLOW,
    longitude = lon,
    latitude = lat,
    date
  )

discharge <-
  full_join(runoff, baseflow, join_by(longitude, latitude, date)) %>% 
  mutate(
    discharge = ((runoff + baseflow) / 1000), # in metres
    discharge = set_units(discharge, "m")
  ) %>% 
  select(latitude, longitude, date, discharge)

rm(runoff, baseflow)

sbf_set_sub("clean", "discharge", rm = TRUE)
sbf_save_datas()
