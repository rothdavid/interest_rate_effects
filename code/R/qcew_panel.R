
# creating panel dataset

# load data

## creates a list for all the df's to be stored in

library("tidyverse")

qcew_list <- list()

for (x in 2018:2023) {
  data <- glue::glue("data/processed/qcew_county_total_{x}.csv")
  qcew_list[[as.character(x)]] <- read_csv(data)
  
}

qcew_panel <- bind_rows(qcew_list)

# count number of rows
 # we are expecting 3274 counties * 4 quarters * 6 years = 78576
a <- qcew_panel |> 
  group_by(year) |> 
  nrow()
 
78576 - a

# counts number of rows for each year and quarter

qcew_panel |> 
  group_by(year, qtr) |> 
  summarise(nrows = n())

# 2018 and 2019 have one less row per quarter

qcew_panel |> 
  group_by(area_fips) |> 
  summarise(nrows = n()) |> 
  filter(nrows < 24)  # 24 = 6 years * 4 qtrs per year
    # output: 02063, 02066, 02261 --- areas in Alaska

# checking for duplicates

qcew_panel |> 
  group_by(year, qtr, area_fips) |> 
  summarise(nrows = n()) |> 
  filter(nrows > 1) 

# load in State Data

area_fips <- read_csv("data/raw/area_fips.csv")                           

area_fips <- area_fips |> 
  mutate(fip = str_sub(county_fipcode, 1, 2)) |> 
  select(state_name, fip) |> 
  distinct()

# join state data
                                                 
qcew_panel1 <- qcew_panel |> 
  mutate(state_fips = str_sub(area_fips, 1, 2)) |> 
  left_join(area_fips, join_by(state_fips == fip)) |> 
  mutate(state_name = case_when(state_fips == "72" ~ "Puerto Rico",
                                state_fips == "78" ~ "U.S. Virgin Islands",
                                TRUE ~ state_name)) |> 
  select(-state_fips)

# Zero and NA Investigation

qcew_panel1 |> 
  filter(is.na(state_name)) |> 
  distinct(area_fips) 

qcew_panel1 |> 
  summarise(zero_emplvl = sum(avg_emplvl == 0),
            na_emplvl = sum(is.na(avg_emplvl)),
            zero_estabs = sum(qtrly_estabs == 0),
            na_estabs = sum(is.na(qtrly_estabs)),
            zero_tot_wage = sum(total_qtrly_wages == 0),
            na_tot_wage = sum(is.na(total_qtrly_wages)),
            zero_week_wage = sum(avg_wkly_wage == 0),
            na_week_wage = sum(is.na(avg_wkly_wage)))

# Found 15 observations with 0's. 

qcew_panel1 |> 
  filter(avg_emplvl == 0, avg_wkly_wage == 0, total_qtrly_wages == 0)

# Created data time object and log vars

qcew_panel1 <- qcew_panel1 |> 
  mutate(date = yq(glue::glue("{year}:{qtr}")),
         log_emplvl = case_when(avg_emplvl != 0 ~ log(avg_emplvl),
                                TRUE ~ NA),
         log_wkly_wage = case_when(avg_wkly_wage != 0 ~ log(avg_wkly_wage),
                                   TRUE ~ NA)) |> 
  arrange(area_fips, date) |>  
  group_by(area_fips) |> 
  mutate(wage_growth_yr = log_wkly_wage - lag(log_wkly_wage, 4),
         wage_growth_qtr = log_wkly_wage - lag(log_wkly_wage, 1),
         emplvl_growth_yr = log_emplvl - lag(log_emplvl, 4),
         emplvl_growth_qtr = log_emplvl - lag(log_emplvl, 1))

qcew_panel1 |> 
  ggplot(mapping = aes(date, wage_growth_yr)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~state_name)

qcew_panel1 |> 
  ggplot(mapping = aes(date, emplvl_growth_yr)) +
  geom_point() +
  geom_line() +
  facet_wrap(~state_name)

qcew_panel1 |>
  ungroup() |> 
  drop_na() |> 
  summarise(a = max(wage_growth_yr),
            b = max(emplvl_growth_yr),
            c = min(wage_growth_yr),
            d = min(emplvl_growth_yr))

qcew_panel1 |> 
  filter(emplvl_growth_yr >= 2)

# removing counties that are undefined 

qcew_panel_data <- qcew_panel1 |> 
  filter(str_sub(area_fips, 3, 5) != "999")

qcew_panel_data |> 
  ungroup() |> 
  drop_na() |> 
  summarise(a = max(wage_growth_yr),
            b = max(emplvl_growth_yr),
            c = min(wage_growth_yr),
            d = min(emplvl_growth_yr))

write_csv(qcew_panel_data, "data/processed/qcew_panel_data.csv")
