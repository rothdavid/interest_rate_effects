# downloads Fed Funds Data

download.file("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=FEDFUNDS&scale=left&cosd=1954-07-01&coed=2026-06-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2026-07-26&revision_date=2026-07-26&nd=1954-07-01", "data/raw/fed_funds.csv", mode = "wb")

fed_funds <- read.csv("data/raw/fed_funds.csv")

# filter for date to be between 2018 and 2023

fed_funds <- fed_funds |> 
  mutate(observation_date = date(observation_date)) |> 
  filter(year(observation_date) >= 2018, year(observation_date) <= 2023) 

write_csv(fed_funds, "data/processed/fed_funds.csv")

  
