# url: https://www.sc-pa.com/downloads/download-data
# Year 2000 to Current Certified Roll -These files contain property
# characteristics data plus values from our most recently certified tax roll to
# the FDOR for real and personal property. Data includes: ownership, addresses,
# exemptions, property characteristics, plus, the file contains sales transfer
# information* from year 2000 to present.
# libraries ----
library(dplyr)
library(ggplot2)
library(scales)
# url <- 'https://www.sc-pa.com/downloads/SCPA_Parcels_Sales_CSV.zip'
# destfile <- './SCPA_Parcels_Sales_CSV.zip'
# download.file(url = url, destfile = destfile)

# import ----
`0riginal` <- readr::read_csv("./data/Parcel_Sales_CSV/Sarasota.csv",
			    name_repair = janitor::make_clean_names
)

# filter to homes only ----
df.0 <-
	`0riginal` |>
	#get rid of 0 values for property value
	filter(sale_amt != 0) |>
	# "Sale qualified as a result of deed examination"
	filter(qual_code == '01') |>
	# housing within residential single family zone
	filter(grepl("^RSF", zoning_1)) |>
	# bedrooms
	filter(bedr >= 1) |>
	# baths
	filter(bath > 0) |>
	#single family or condos
	filter(grepl("^01|^04", stcd)) |>
	#omit non-ascii character from name field
	mutate(name1 = gsub(" \xa7119.071FS", "", name1))

# create features ----
df.1 <-
	df.0 |>
	#create year
	mutate(sale_date = as.Date(sale_date, format = "%m/%d/%Y")) |>
	mutate(year = substr(sale_date, start = 0, stop = 4)) |>
	#convert to character
	mutate(year = as.integer(year)) |>
	# create factor for condo vs. home
	mutate(land_use = ifelse(grepl("^01", stcd),
				 "single_family",
				 "condo")) |>
	mutate(land_use = factor(land_use, levels = c("single_family",
						      "condo"))) |>
	# non-resident owner based on zip
	mutate(home_owner = ifelse(zip == loczip,
				     "full-time",
				     "part-time")) |>
	mutate(home_owner = factor(home_owner, levels = c('full-time',
							  'part-time'))) |>
	# age of home at sale
	mutate(age_at_sale = year - yrbl) |>
	# age now
	mutate(age_now = as.integer(substr(Sys.Date(), start = 0, stop = 4)) - yrbl) |>
	mutate(age_class = ggplot2::cut_interval(
		age_now,
		n = 5,
		labels = c("antiquated",
			   "very_old",
			   "old",
			   "modern",
			   'new')
	)) |>
	# water or no water
	mutate(water = ifelse(grepl("MAIN", gulfbay), "off-water", "on-water")) |>
	mutate(water = factor(water, levels = c('off-water', 'on-water'))) |>
	#convert bedrooms to factor variable
	mutate(bedr = as.character(bedr)) |>
	mutate(bedr = forcats::fct_lump_n(bedr, n = 7)) |>
	# convert bathrooms to factor variable
	mutate(bath = as.character(bath)) |>
	mutate(bath = forcats::fct_lump_n(bath, n = 5)) |>
	# homestead allowance
	mutate(homestead = ifelse(grepl("X", homestead), "permanent", "temporary")) |>
	mutate(homestead = factor(homestead, levels = c("permanent", "temporary")))
# # adjust for inflation
df.2 <-
	df.1 |>
	#adjust for inflation
	mutate(sale_amt_infl_adj = priceR::afi(sale_amt,
				       from_date = year,
				       country = "US",
				       to_date = 2020)) |>
	#price per sq. foot
	mutate(usd_per_sq_ft = sale_amt_infl_adj / living) |>
	mutate(usd_per_sq_ft = usd_per_sq_ft |> round(0)) |>
	filter(is.finite(usd_per_sq_ft))

# select ----
my_columns <- c(# address for tax bill
	'account',
	'name1',
	'name_add4',
	'name_add5',
	'city',
	'state',
	'zip',
	'country',
	# property location (Sarasota Co.)
	'locn', # location number
	'locs', # location street
	'locd', # location direction
	'unit',
	'loccity',
	'locstate',
	'loczip',
	'stcd', # land use codes 0402 == condo duplex or villa
	'nghb', # 1209 neighborhoods
	'subd', # 5108 subdivisions
	'txcd', # 66 different tax codes
	'municipality',
	'gulfbay', # 14 with 'MAIN' being on non-navigable waterway
	'homestead', # either NA or 'X'
	'zoning_1', #161 possible classifications
	'sale_amt', #finally!
	'sale_date',
	# 53 possible '01' 145621 Sale qualified as a result of deed examination
	'qual_code',
	'grnd_area', #ground area
	'living', #living area
	'bedr', #bedrooms eliminate 1BR or 0BR 78000
	'bath', #bath eliminate 0 bath 67311
	'halfbath',
	#'livunits', #don't know about that
	'yrbl', #year built
	'lsqft', #perimeter?
	'assd',
	'year',
	'land_use',
	'home_owner',
	'usd_per_sq_ft',
	'age_at_sale',
	'age_now',
	'age_class',
	'water',
	'sale_amt_infl_adj'
)
df.3 <-
	df.2 |>
	select(all_of(my_columns))
# save sarasota homes data ----
saveRDS(df.3, file = "./sarasota_homes.rds")
