library(tidyverse)
library(ggrepel)

tuesdata <- tidytuesdayR::tt_load(2023, week = 50)
holiday_movies <- tuesdata$holiday_movies
holiday_movie_genres <- tuesdata$holiday_movie_genres
#
plot_data_hm = holiday_movies |>
  select(-genres) |>
  inner_join(
    holiday_movie_genres
  ) |>
  mutate(
    year = case_when(
      year < 1925 ~ "<1925", 
      year %in% 1925:1950 ~ '1925-1950',
      year %in% 1950:1975 ~ '1950-1974',
      year %in% 1975:2000 ~ '1975-1999',
      year > 2000 ~ '2000+'
    )
  ) 

runtime_ratings_holiday_movie = plot_data_hm |>
  ggplot(
    aes(x = runtime_minutes, y = average_rating, colour = as.factor(year))
  ) +
  geom_point() +
  scale_color_brewer(palette = 'Dark2') +
  facet_wrap(.~genres, scales = 'free') +
  geom_text_repel(
    data = subset(plot_data_hm, runtime_minutes > 200 | average_rating > 9.5),
    aes(label = simple_title)
  ) +
  labs(
    title = "Weighted Average ~ Primary Runtime", 
    y = "Weighted Average", x = "Minutes",
    caption = "https://github.com/akhapwoyaco"
  ) +
  
  theme_bw() +
  theme(
    legend.position = 'top',
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = 'bold')
  )
#
runtime_ratings_holiday_movie
ggsave(
  plot = runtime_ratings_holiday_movie, filename = "runtime_ratings_holiday_movie.jpeg",
  width = 40, height = 40, units = "cm", dpi = 400)
