#
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load(2022, week = 36)
inventories <- tuesdata$inventories
#
inventories <- tuesdata$inventories.csv
sets <- tuesdata$sets.csv
inventory_sets <- tuesdata$inventory_sets.csv
#
all_df <- left_join(inventories, inventory_sets, by = "set_num") |>
  left_join(sets, by = "set_num") 
#
unique(all_df)
head(all_df)
#
aggregate(
  quantity~version, data = all_df, FUN = mean, na.rm = TRUE
)
#
aggregate(
  quantity~version, data = all_df, FUN = sum, na.rm = TRUE
)
#
year_version_quantity_data <- aggregate(
  quantity~version+year, data = all_df, FUN = sum, na.rm = TRUE
) |> mutate_at(vars(version), factor)
#
years_data = sort(unique(year_version_quantity_data$year))
(year_max = max(years_data))
(year_min = min(years_data))
#
year_version_quantity_r <- year_version_quantity_data |> 
  ggplot(aes(x = year, y = quantity, color = version)) + 
  geom_point() + 
  scale_x_continuous(breaks = seq(year_min, year_max, by = 1)) +
  labs(title = 'Total Lego Quantity by Version and Year', 
       subtitle = 'Source: https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/', 
      x = 'Year', y = 'Total Quantity', 
      alt = 
      'Lego Data on Quantity was aggregated by version and year through 
      summation and plot made with x-axis = Year, 
      y-axis = Total Quantity, coloring based on version',
      caption = "https://github.com/akhapwoyaco") + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )
#
year_version_quantity_r
#
ggsave(filename = 'year_version_quantity_r.png', 
       plot = year_version_quantity_r, 
       width = 40, height = 30, dpi = 500)
#
