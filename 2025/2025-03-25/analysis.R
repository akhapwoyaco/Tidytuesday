#
library(tidyverse)
library(readr)
report_words_clean <- read_csv("report_words_clean.csv")
head(report_words_clean)
#
simple_count_plot = report_words_clean |> 
  mutate(
    word = word |> tolower() |> trimws()
  ) |>
  group_by(year, word) |>
  summarise(count = n()) |> 
  arrange(desc(count)) |> 
  # View()
  ggplot(aes(x = year, y = count)) +
  geom_point()
#
simple_count_plot
#
ggsave(plot = simple_count_plot, filename = "simple_count_plot.jpeg",
width = 25, height = 20, dpi = 450, units = 'cm')

