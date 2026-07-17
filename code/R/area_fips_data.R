
# This script downloads the State, County, and City Data

fips_data <- glue::glue("https://data.transportation.gov/resource/eek5-pv8d.csv?$limit=50000") 

destination <- glue::glue("data/raw/area_fips.csv")

download.file(fips_data, destination, mode = "wb")