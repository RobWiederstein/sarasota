# read in parcel sale ----
homes <- readRDS("./data/sarasota_homes.rds")
# create subd table  ----
subdivisions <-
	homes |>
	select(account,
	       municipality,
	       subd,
	       sale_amt_infl_adj,
	       living,
	       usd_per_sq_ft,
	       age_now) |>
	group_by(municipality, subd) |>
	summarize(avg_sale = mean(sale_amt_infl_adj, na.rm = T, trim = .5) |> round(0),
		  avg_living = mean(living, na.rm = T, trim = .5) |> round(0),
		  avg_usd_sq_ft = mean(usd_per_sq_ft, na.rm = T, trim = .5) |> round(0),
		  avg_age = mean(age_now, na.rm = T, trim = .5) |> round(0),
		  lots = n()) |>
	filter(lots > 20) |>
	filter(subd != "0000") |>
	arrange(desc(lots)) |>
	ungroup()
subd_codes <- readr::read_csv('./Parcel_Sales_CSV/SubdivisionCodes.csv',
			      col_names = c("subd", "desc"))
subdivisions1 <- left_join(subdivisions, subd_codes, by = "subd") |>
	select(municipality, desc, subd:lots)
# merge subdivision boundaries ----
plat <- sf::st_read(
	dsn = "./PlatBoundary",
	layer = "PlatBoundary") |>
	sf::st_transform('+proj=longlat +datum=WGS84')
#reduce file size
plat <- rmapshaper::ms_simplify(plat)
plat.1 <-
	plat |>
	select(max_sub_id, acre, geometry) |>
	rename(subd = max_sub_id)
sbdv <- merge(plat.1, subdivisions1, by = "subd")
# save object ----
saveRDS(sbdv, file = "subdivisions.rds")
