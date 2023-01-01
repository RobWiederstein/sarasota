#epa beacon database report
beaches <-
	readr::read_csv("./data/epa_beaches/beach_attributes.csv",
			   name_repair = ~janitor::make_clean_names(.x)
			   ) |>
	select(year, state, county, beach_name,
	       beach_ownership_beach_accessibility,
	       beach_length_mi, start_latitude,
	       start_longitude, end_latitude,
	       end_longitude) |>
	rowwise() |>
	mutate(lat = mean(start_latitude, end_latitude),
	       lng = mean(start_longitude, end_longitude)) |>
	select(year:beach_length_mi, lat, lng) |>
	rename(beach = beach_name,
	       access = beach_ownership_beach_accessibility,
	       length = beach_length_mi) |>
	mutate(length = as.character(length)) |>
	mutate(length = stringr::str_pad(length, width = 5, side = "right",
	 				 pad = "0")) |>
	mutate(length = paste0(length, " mi."))
saveRDS(beaches, "./sarco_beaches.rds")
