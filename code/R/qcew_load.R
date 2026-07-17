
# download QCEW data from 2018--2023

qcew_download <- function(YEAR){
  
  qcew_data(YEAR)
  qcew_processed_data(YEAR)
  qcew_check(YEAR)
  
}
