library(sf)
library(RColorBrewer)
# https://data-sarco.opendata.arcgis.com/datasets/
# sarasotacountyboundary/explore
#import
county <- sf::st_read(dsn = "./SarasotaCountyBoundary",
		      layer = "SarasotaCountyBoundary") |>
	sf::st_transform('+proj=longlat +datum=WGS84')
#reduce
sarasota_boundary <- rmapshaper::ms_simplify(county)
# add town names, color and popup
names <- data.frame(municipali = c("CS", "CV", "SC", "CNP", "TLK"),
		    muni_names = c("City of Sarasota",
		    "City of Venice",
		    "Sarasota County",
		    "City of North Port",
		    "Town of Longboat Key"),
		    color = c(brewer.pal(5, "Dark2")))
# merge
sarasota_boundary <- merge(sarasota_boundary, names, by = "municipali")
#save
saveRDS(sarasota_boundary, "./sarasota_boundary.rds")
