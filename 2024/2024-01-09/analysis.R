#
library(tidyverse)
canada <- read_csv("canada_births_1991_2022.csv")
nhl_player_births <- read.csv('nhl_player_births.csv')
#
country = canada |> 
  group_by(month) |> 
  summarise(total = sum(births)) |>
  mutate(
    percentage = 100*total/sum(total),
    months = month.abb[month],
    category = "CANADA"
  ) |> select(percentage, months, category)
#
nhl = nhl_player_births |>
  group_by(birth_month) |> 
  summarise(total = n()) |>
  mutate(
    percentage = 100*total/sum(total),
    months = month.abb[birth_month],
    category = "NHL"
  ) |> select(percentage, months, category)
#
country_nhl = bind_rows(
  country,
  nhl) |>
  mutate(
    months = factor(months, levels = month.abb)
  )
country_nhl
#
country_nhl |>
  ggplot(
    aes(x = percentage, y = months, color = category)
  ) +
  geom_point() +
  geom_text(
    aes(label = paste(round(percentage, 1), "%")), hjust = 0.5, vjust = -1
  )
#
birth_percentages_plot = country_nhl |>
  pivot_wider(
    id_cols = months, names_from = category, values_from = percentage
  ) |> #head()
  ggplot() +
  geom_point(aes(y = months, x = CANADA)) +
  geom_text(aes(y = months, x = CANADA, label = paste(round(CANADA, 1), "%")),
            hjust = 0.5, vjust = -1, colour = 'red') +
  geom_point(aes(y = months, x = NHL)) +
  geom_text(aes(y = months, x = NHL, label = paste(round(NHL, 1), "%")),
            hjust = 0.5, vjust = -1, colour = 'blue') +
  geom_linerange(
    aes(y = months, xmin = CANADA, xmax = NHL)
  ) +
  theme_light() +
  labs(
    y = "Month of Birth", x = "Percentage of Births (%)",
    title = "Are Canadian NHL Players More Likely to be Born Early in the Year?",
    subtitle = "Comparing the distribution of birth months between Canadian NHL Players and Canada in general") +
  scale_x_continuous(labels = scales::unit_format(suffix = "%")) +
  annotate("text", x = 9.5, y = 12, colour = "blue", label = "Canadian NHL Players Birth Month Distribution") +
  annotate("text", x = 9.5, y = 11.5, colour = "red", label = "Canadian Birth Month Distribution") +
  theme(
    legend.position = 'none'
  )
#
birth_percentages_plot
#
ggsave(
  filename = "birth_percentages_plot.jpeg",
  plot = birth_percentages_plot,  dpi = 600,
  width = 30, height = 25, units = 'cm'
)
#