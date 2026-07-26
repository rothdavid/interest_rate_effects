# overall changes

qcew_panel_data |> 
  group_by(date) |> 
  summarise(avg_wage = mean(wage_growth_qtr, na.rm = TRUE)) |> 
  ggplot(mapping = aes(x = date, y = avg_wage)) +
  geom_point() +
  geom_line()

qcew_panel_data |> 
  group_by(date) |> 
  summarise(avg_wage = mean(wage_growth_yr, na.rm = TRUE)) |> 
  ggplot(mapping = aes(x = date, y = avg_wage)) +
  geom_point() +
  geom_line()

qcew_panel_data |> 
  group_by(date) |> 
  summarise(empl = mean(emplvl_growth_qtr, na.rm = TRUE)) |> 
  ggplot(mapping = aes(x = date, y = empl)) +
  geom_point() +
  geom_line()

qcew_panel_data |> 
  group_by(date) |> 
  summarise(empl = mean(emplvl_growth_yr, na.rm = TRUE)) |> 
  ggplot(mapping = aes(x = date, y = empl)) +
  geom_point() +
  geom_line()

# graphs by districts

qcew_panel_district_data |> 
  count(fed_district)

qcew_panel_district_data |> 
  group_by(date, fed_district) |> 
  summarise(avg_wage = mean(wage_growth_qtr, na.rm = TRUE)) |> 
  ggplot(mapping = aes(x = date, y = avg_wage, color = fed_district, group = fed_district)) +
  geom_point() +
  geom_line()

qcew_panel_district_data |> 
  group_by(date, fed_district) |> 
  summarise(avg_wage = mean(wage_growth_yr, na.rm = TRUE)) |> 
  ggplot(mapping = aes(x = date, y = avg_wage, color = fed_district, group = fed_district)) +
  geom_point() +
  geom_line()

qcew_panel_district_data |> 
  group_by(date, fed_district) |> 
  summarise(empl = mean(emplvl_growth_qtr, na.rm = TRUE)) |> 
  ggplot(mapping = aes(x = date, y = empl, , color = fed_district, group = fed_district)) +
  geom_point() +
  geom_line()

qcew_panel_district_data |> 
  group_by(date, fed_district) |> 
  summarise(empl = mean(emplvl_growth_yr, na.rm = TRUE)) |> 
  ggplot(mapping = aes(x = date, y = empl, , color = fed_district, group = fed_district)) +
  geom_point() +
  geom_line()
