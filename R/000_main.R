#fetch and save sarasota property data
source("./R/005_download_sarco_sales.R")

#create sarco home sales 1900 to date
source("./R/010_create_sarco_home_sales_all.R")

#create sarasota home sales point data current
source("./R/020_create_sarco_home_sales_current.R")

#create sarasota county subdivision data
source("./R/030_create_sarco_subdivisions.R")

#create sarasota zip code boundaries
source("./R/040_create_sarco_zip_codes.R")

#create sarasota beaches point data
source("./R/050_create_sarco_beaches.R")

#create sarasota government boundaries
source("./R/060_create_sarco_govt_boundaries.R")

#update app
rsconnect::deployApp(forceUpdate = T)
