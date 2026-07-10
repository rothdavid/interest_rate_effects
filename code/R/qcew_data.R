dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

options(timeout = 1200)

qcew_data <- function(YEAR) {
  
  url <- glue::glue("https://data.bls.gov/cew/data/files/{YEAR}/csv/{YEAR}_qtrly_singlefile.zip")
  
  destination <- glue::glue("data/raw/download_qcew_{YEAR}_singlefile.zip")
  
  download.file(url, destfile = destination, mode = "wb")
  
  message("Downloaded ", destination)
  
  if (file.exists(destination)) {
    file.remove(destination)
  }
}


qcew_data(2023)

