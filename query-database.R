source("header.R")

sbf_load_datas_from_db()

water_temp <- drop_units(water_temp)
discharge <- drop_units(discharge)
air_temp <- drop_units(air_temp)

sbf_set_sub("query")
sbf_save_datas()
