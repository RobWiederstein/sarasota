library(tidyr)
library(dplyr)
library(purrr)
# url = https://data-sarco.opendata.arcgis.com/datasets/addresspoint/explore
#import gis data
addresses <- sf::st_read(dsn = "./data/AddressPoint",
			 layer = "AddressPoint") |>
	sf::st_transform('+proj=longlat +datum=WGS84')
#create address1 column for merge
add_1 <-
	addresses |>
	mutate(postalcomm = toupper(postalcomm)) |>
	unite(col = address1,
	      address, postalcomm, state, zip,
	      sep = " ",
	      remove = F) |>
	select(address1, geometry) |>
	mutate(lng = unlist(map(addresses$geometry,1)),
	       lat = unlist(map(addresses$geometry,2)))
#import sale data
homes <- readRDS("./sarasota_homes.rds")
#create address1 column
homes_1 <-
	homes |>
	unite(col = "address1",
	      locn, locd, locs,  loccity, locstate, loczip,
	      sep = " ",
	      remove = FALSE) |>
	mutate(address1 = gsub(" NA ", " ", address1)) |>
	# filter to sales that occurred within one year of latest
	# available sale
	filter(sale_date >= max(sale_date)- 730)
#merge point data with sale data 88% merge rate
df <- merge(homes_1, add_1, by = "address1")
df <-
	df |>
	dplyr::filter(sale_amt < 1e6) |> # < 345 sales greater than $1 million
	dplyr::arrange(sale_amt) |>
	dplyr::select(-geometry) |>
	tidyr::drop_na(lng, lat)

saveRDS(df, "./sarasota_prices.rds")
