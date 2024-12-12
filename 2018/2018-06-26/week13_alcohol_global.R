#
library(readr)
library(ggplot2)
library(dplyr)
library(tidytuesdayR)
#
tuesdata <- tidytuesdayR::tt_load('2018-06-26')
week13_alcohol_global <- tuesdata$week13_alcohol_global#read_csv("week13_alcohol_global.csv")
head(week13_alcohol_global)
#

world_map_data <- map_data('world') |>
  filter(region != "Antarctica") |>
  fortify()

plot1 <- ggplot() + 
  geom_map(
    data = world_map_data, map = world_map_data,
    aes(x = long, y = lat, group = group, map_id = region),
    fill = "white", color = "#7f7f7f", size = 0.5
  ) + 
  geom_map(
    data = week13_alcohol_global, map = world_map_data,
    aes(fill = beer_servings, map_id = country),
    color = "#7f7f7f", size = 0.5
  ) +  
  coord_map(
    "rectangular", lat0 = 0, 
    xlim = c(-180, 180), ylim = c(-60, 90)) +
  scale_fill_continuous(
    low = 'thistle2', high = 'darkred', guide = 'colorbar'
  ) + 
  scale_y_continuous(breaks = c())  + 
  scale_x_continuous(breaks = c())  + 
  #labs(fill = "", title = "") +
  theme_bw() + 
  theme(
    legend.direction = 'horizontal',
    legend.position = 'inside',
    legend.position.inside = c(0.15, 0.2),
    legend.title = element_blank(),
    axis.title = element_blank()
  )
#
plot1
ggsave(plot = plot1, filename = "world_map_data_alcohol.png", 
       width = 30, height = 20, units = "cm", dpi = 350)

