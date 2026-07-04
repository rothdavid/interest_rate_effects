dir.create("data/raw", recursive = T, showWarnings = F)

qcew_data <- function(YEAR, QUARTER){
  
  url <- https://data.bls.gov/cew/data/files/YEAR/csv/YEAR_qtrQUARTER_singlefile.zip
  
  download.file(download_qcewYEARQUARTER, destfile = "url", mode = "wb")
  
  print("Downloaded " download_qcewYEARQUARTER)
}
