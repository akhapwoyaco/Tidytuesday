
# get data
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load(2020, week = 32)
energy_types <- tuesdata$energy_types
country_totals <- tuesdata$country_totals
head(country_totals); head(energy_types)

dim(country);dim(energy)

energy_types[complete.cases(energy_types$country_name),] |> View() #there are country_names missing

energy_total <- energy_types |> 
  select(-level, -country_name) |> 
  pivot_longer(
    cols = `2016`:`2018`,
    names_to = "year",
    values_to = "total net production" ) |> #get sum of production by country n by year 
  count(country, year, wt = `total net production`, name  = 'total', sort = FALSE)

#2017
energy_percent <- energy_types |> 
  select(-level, -country_name) |>
  pivot_longer(
    cols = `2016`:`2018`,
    names_to = "year",
    values_to  = "total net production") |>
  inner_join(
    energy_total, by = c("country"="country", "year"="year")) |> 
  mutate( 
    percent = (`total net production`/total)*100, 
    percent_sig = if_else(
      type == "Conventional thermal", percent*(-1), percent)
  ) |>
  select(-`total net production`,-total) |> 
  filter(year == 2017)

plot2017 = energy_percent |> 
  ggplot(
    aes(x = reorder(country, -percent_sig), y = percent_sig, fill = type) ) + 
  geom_col(position = "identity", size = 0.1) + 
  scale_fill_brewer(palette = "Dark2") + 
  geom_text(
    aes(label = country), 
    subset(energy_percent, percent_sig < 0 | percent_sig > 99),
    nudge_y = -3
  ) + 
  ggthemes::theme_fivethirtyeight() +
  theme(
    axis.title.y = element_blank(), 
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.line.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted"), 
    panel.grid.minor.y = element_line(linetype = "dotted"), 
    legend.position = 'inside',
    legend.position.inside = c(0.8,0.9), 
    legend.background = element_blank(), 
    legend.key = element_blank()
  ) + 
  guides(fill = guide_legend(title = NULL)) + 
  labs(
    title = "Energy Production in Europe: 2017", 
    subtitle = "Percentage of Type of Energy to Total Production per Country", 
    caption = "https://github.com/akhapwoyaco"
  )
plot2017
#
ggsave(
  "plot2017.png", plot2017, width = 25, height = 20,
  dpi = 550, units = "cm")

#