# host = https://www.sc-pa.com/downloads/download-data/
dir.create("./temp")
url <- "https://www.sc-pa.com/downloads/SCPA_Parcels_Sales_CSV.zip"
destfile <- "./temp/SCPA_Parcels_Sales_CSV.zip"
download.file(url = url, destfile = destfile, mode = "wb")
unzip(destfile, exdir = "./temp")
from <- "./temp/Parcel_Sales_CSV/"
to <- "./data/"
file.copy(from = from, to = to, overwrite = T, recursive = T)
unlink("./temp", recursive = T, force = T)
