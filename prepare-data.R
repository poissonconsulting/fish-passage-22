source("header.R")

sbf_set_sub("query")
sbf_load_datas()

D <- sbf_load_object("downstream_hydrologic_distance", sub = "distance/temp")
dist_sites <- rownames(D)

# Sites
sites <- 
  water_temp_site %>% 
  tibble() %>% 
  select(site)

# Date ref for bounding dates
dates_sites_all <-
  tibble(
    date = seq(bounding_dates[1], bounding_dates[2], by = "day")
  ) %>% 
  mutate(
    annual = dtt_year(date),
    day = as.integer(date),
    day = day - min(day) + 1,
    week = day %/% 7 + 1,
  ) %>% 
  cross_join(sites)

first_date_week <- 
  dates_sites_all %>% 
  distinct(date, week) %>% 
  group_by(week) %>% 
  slice_head() %>% 
  ungroup()

# Keep all water temp except for "fail"
water_temp %<>%
  filter(flag != "F") %>% 
  full_join(dates_sites_all, join_by(date, site))

water_temp %<>% 
  # filter(week %in% date_ref$week) %>% 
  group_by(site, week) %>% 
  summarize(
    water_temp = mean(temp),
    annual = first(annual),
    .groups = "drop"
  ) %>% 
  arrange(site, week)

# Aggregate air temp data by week
air_temp %<>%
  full_join(dates_sites_all, join_by(site, date)) %>%
  group_by(site, week) %>% 
  summarize(
    air_temp = mean(air_temp),
    .groups = "drop"
  ) %>% 
  arrange(site, week)

# Aggregate discharge data by week, match to water temp sites
closest_sites <- 
  water_temp_site %>% 
  st_join(
    discharge_site,
    join = st_nearest_feature
  ) %>% 
  tibble() %>% 
  select(site, discharge_site)

discharge %<>% 
  right_join(closest_sites, join_by(discharge_site), relationship = "many-to-many") %>% 
  left_join(dates_sites_all, join_by(date, site)) %>%
  group_by(discharge_site, week, site) %>% 
  summarize(
    discharge = mean(discharge),
    .groups = "drop"
  )

elev <- 
  water_temp_site %>% 
  drop_units() %>% 
  tibble() %>% 
  select(site, elev)

# Add missing values for water temp by expanding to every site-week combo
water_temp %<>% 
  left_join(air_temp, join_by(site, week)) %>% 
  left_join(discharge, join_by(site, week)) %>% 
  left_join(elev, join_by(site)) %>% 
  left_join(first_date_week, join_by(week)) %>% 
  mutate(
    site_remains = if_else2(site %in% dist_sites, TRUE, FALSE)
  ) %>% 
  filter(site_remains) %>% 
  mutate(
    site = factor(site),
    week = factor(week)
  ) %>%
  ### **Must be arranged in this order for the model**
  arrange(week, site)

sbf_set_sub("prepare")
sbf_save_datas()

if (FALSE) {
  sbf_compare_data_archive()
}