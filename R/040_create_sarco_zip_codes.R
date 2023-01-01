library(sf)
library(RColorBrewer)
# https://data-sarco.opendata.arcgis.com/datasets/
#import
zips <- sf::st_read(dsn = "./data/ZipCode",
		      layer = "ZipCode") |>
	sf::st_transform('+proj=longlat +datum=WGS84') |>
	select(objectid, postalcomm, zip_code, geometry) |>
	mutate(geometry = rmapshaper::ms_simplify(geometry))
saveRDS(zips, file = "./sarco_zips.rds")
