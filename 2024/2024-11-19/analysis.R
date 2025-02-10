library(readr)
episode_metrics <- read_csv("episode_metrics.csv")
View(episode_metrics)
#
library(ggplot2)
library(ggrepel)
season_episode_que_excl_labs = episode_metrics |>
  select(season, episode, question_ratio,exclamation_ratio) |>
  filter(question_ratio > 0.25 | exclamation_ratio > 0.25) |>
  mutate(
    ssn_eps = str_c("Season: ", season, ", Episode: ", episode, sep = '')
  )


bobs_burger_exclamation_question_ratios_plot = episode_metrics |>
  ggplot(aes(x = question_ratio, y = exclamation_ratio)) +
  geom_point() +
  geom_point(
    data = season_episode_que_excl_labs,
    aes(
      x = question_ratio, y = exclamation_ratio,
      color = ssn_eps)
  ) +
  geom_text_repel(
    data = season_episode_que_excl_labs,
    aes(
      x = question_ratio, y = exclamation_ratio,
      label = ssn_eps, color = ssn_eps), nudge_x = 0.001, nudge_y = 0.001) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(
    title = "Bob's Burgers",
    subtitle = "The proportion of lines of dialogue that contain at least one exclamation point ('!') ~ least one question mark ('?')",
    x = "Question Ratio", y = "Exclamation Ratio",
    caption = c("tidytuesdayR::tt_load(2024, week = 47)", "https://github.com/akhapwoyaco/")
  ) +
  theme(
    axis.text = element_text(face = 'bold'),
    axis.title = element_text(face = 'bold'),
    plot.title = element_text(face = 'bold', hjust = 0.5),
    plot.subtitle = element_text(face = 'bold', hjust = 0.5),
    legend.position = 'none',
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.direction = 'horizontal',
    plot.caption = element_text(hjust = c(1,0))
  )
#
bobs_burger_exclamation_question_ratios_plot
#
ggsave(
  filename = "bobs_burger_exclamation_question_ratios_plot.jpeg",
  plot = bobs_burger_exclamation_question_ratios_plot,  dpi = 600,
  width = 30, height = 25, units = 'cm'
)
#
