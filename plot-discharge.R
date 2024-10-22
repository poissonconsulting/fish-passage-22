source("header.R")

sbf_set_sub("query")
sbf_load_datas()

discharge %<>% 
  group_by(latitude, longitude) %>% 
  mutate(
    station_id = cur_group_id(),
    station_id = factor(station_id)
  )

gp <- ggplot(discharge) +
  geom_line(aes(x = date, y = discharge, colour = station_id, group = station_id), alpha = 0.2) +
  ylab("Discharge") +
  xlab("Date") +
  labs(colour = "Station ID") +
  theme(legend.position = "none") +
  NULL

sbf_open_window(8, 10)
sbf_print(gp)

sbf_save_plot(
  x_name = "discharge",
  report = FALSE,
  caption = "Mean daily discharge by date and site"
)
