source("header.R")

sbf_set_sub("clean", "solar-rad")
sbf_load_datas()

water_temp_site <- sbf_load_data("water_temp_site", sub = "tidy/water-temp")
water_temp_site_upstream <- sbf_load_data("water_temp_site_upstream", sub = "tidy/water-temp")

solar_rad %<>%
  group_by(longitude, latitude) %>% 
  mutate(
    cell_id = cur_group_id(),
    cell_id = as.character(cell_id)
  ) %>% 
  ungroup()

solar_rad_site <- 
  solar_rad %>% 
  select(cell_id, Longitude = longitude, Latitude = latitude) %>% 
  distinct() %>% 
  arrange(cell_id) %>% 
  ps_longlat_to_sfc()

# Filter to areas closest to water temp sites
closest_site_water_temp <- 
  ps_nearest_feature(water_temp_site, solar_rad_site, dist_col = "dist") %>% 
  as_tibble() %>% 
  select(site, cell_id, dist)

max_dist <- max(closest_site_water_temp$dist)
message("maximum distance between grid and water temp sites: ", max_dist, " m")

solar_rad %<>%
  filter(cell_id %in% closest_site_water_temp$cell_id) %>%
  left_join(closest_site_water_temp, by = "cell_id", relationship = "many-to-many") %>%
  select(
    cell_id, site, solar_rad, date, time
  )

rm(max_dist, closest_site_water_temp, water_temp_site)

sbf_set_sub("tidy", "solar-rad", rm = TRUE)
sbf_save_datas()

if (FALSE) {
  sbf_compare_data_archive()
}
