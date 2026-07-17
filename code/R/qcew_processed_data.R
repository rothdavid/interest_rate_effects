
# This scipt takes raw QCEW data for each year and filters the data to a county level

qcew_processed_data <- function(YEAR) {
  file <- glue::glue("data/raw/download_qcew_{YEAR}_singlefile.zip")
  qcew_raw <- read_csv(file)
  qcew_county_total_YEAR <- qcew_raw |> 
# filtering for county
     filter(industry_code == "10", agglvl_code == 70, 
           size_code == 0, own_code == 0) |> 
# creates average employment for each quarter
    mutate(avg_emplvl = (month1_emplvl + month2_emplvl
                         + month3_emplvl) / 3) |> 
    select(area_fips, year, qtr, qtrly_estabs, total_qtrly_wages, 
           avg_wkly_wage, avg_emplvl)
# saving the processed data 
  name <- glue::glue("data/processed/qcew_county_total_{YEAR}.csv")
  write_csv(qcew_county_total_YEAR, name)
}

