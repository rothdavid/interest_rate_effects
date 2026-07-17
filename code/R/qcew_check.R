
# test qcew data processing

qcew_check <- function(YEAR){
  file <- glue::glue("data/processed/qcew_county_total_{YEAR}.csv")
  data <- read.csv(file)
  # counts number of rows
  nrow(data)
  data |> 
    group_by(qtr) |> 
    summarize(n_rows = n())
  data |> 
    # checking for one row per quarter for each county 
    group_by(qtr, area_fips) |> 
    count() |> 
    filter(n >1)
}


