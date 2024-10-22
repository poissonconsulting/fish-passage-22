source("header.R")

sbf_set_sub("clean", "discharge")
sbf_load_datas()

discharge %<>% 
  group_by(latitude, longitude) %>% 
  mutate(discharge_site = str_c("dis_", cur_group_id())) %>% 
  ungroup()

discharge_site <- 
  discharge %>% 
  distinct(discharge_site, latitude, longitude) %>% 
  rename(
    Latitude = latitude,
    Longitude = longitude
  ) %>% 
  ps_longlat_to_sfc()

sbf_set_sub("tidy", "discharge", rm = TRUE)
sbf_save_datas()
