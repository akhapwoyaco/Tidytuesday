
library(tidyverse)
library(readr)
#
emissions <- read_csv("emissions.csv")
View(emissions)
#
emissions_uncategorized <- emissions |> 
  group_by(year, commodity) |> 
  summarise(
    total_emissions_MtCO2e = sum(total_emissions_MtCO2e)
  ) |> 
  ggplot(
    aes(x = year, y = total_emissions_MtCO2e, fill = commodity)
  ) + 
  labs(
    y = 'TOTAL EMISSIONS ( MtCO2_e )', x = 'YEAR',
    title = 
  ) +
  geom_area() + theme_classic() + 
  theme(
    panel.grid.major.y = element_line(),
    legend.position = 'bottom',
    legend.title = element_blank()
    )
#
#
#
emissions_categorized <- emissions |> 
  mutate(
    commodity = case_when(
      commodity == "Oil & NGL" ~ "Oil & NGL", 
      commodity == "Natural Gas" ~ "Gas" , 
      commodity == "Sub-Bituminous Coal" ~ "Coal",
      commodity == "Metallurgical Coal" ~ "Coal", 
      commodity == "Bituminous Coal" ~ "Coal",
      commodity == "Thermal Coal" ~ "Coal", 
      commodity == "Anthracite Coal" ~ "Coal",
      commodity == "Cement" ~ "Cement",
      commodity == "Lignite Coal" ~ "Coal",
      .default = commodity
    ) |> toupper()
  ) |>
  group_by(year, commodity) |> 
  summarise(
    total_emissions_MtCO2e = sum(total_emissions_MtCO2e)
  ) |>
  ggplot(
    aes(x = year, y = total_emissions_MtCO2e, fill = commodity)
  ) + 
  labs(
    y = 'TOTAL EMISSIONS ( MtCO2_e )', x = 'YEAR'
  ) +
  geom_area() +
  scale_x_continuous(n.breaks = 20, expand = c(0,0)) +
  theme_classic() + 
  theme(
    panel.grid.major.y = element_line(),
    legend.position = 'bottom',
    legend.title = element_blank()
  )
#
emissions_uncategorized
emissions_categorized
#
