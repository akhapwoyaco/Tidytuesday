library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2021, week = 23)

summar_y <- tuesdata$summary
challenges <- tuesdata$challenges
#
viewer_data_long = summar_y |> 
  select(season, contains('viewers'), rank) |>
  pivot_longer(
    cols = viewers_premier:viewers_mean,
    values_to = 'values', names_to = 'viewers'
  ) |>
  mutate(
    viewers = str_replace(viewers, "viewers_", "") |> str_to_title()
  ) |>
  mutate(
    viewers = factor(viewers, levels = c("Premier", "Mean", "Finale", "Reunion"))
  ) 
#
viewers_col = viewer_data_long |>
  ggplot(aes(x = season, y = values, fill = viewers)) +
  geom_col(position = position_dodge()) +
  scale_fill_brewer(palette = 'Dark2') +
  theme_classic() +
  scale_y_continuous(labels = scales::unit_format(unit = 'M'), expand = c(0,0)) +
  scale_x_continuous(n.breaks = 40, expand = c(0,0)) +
  labs(
    caption = "https://github.com/akhapwoyaco", 
    title = "US Survivor 40 Seasons Viewership", 
    subtitle = "TidyTuesday: 2021-06-01    https://github.com/doehm/survivoR") +
  theme(
    legend.position = 'inside',
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.position.inside = c(0.6, 0.75),
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.y = element_line()
  )
viewers_col
ggsave(
  plot = viewers_col, filename = "viewers_col.jpeg", 
  width = 35, height = 25, units = "cm", dpi = 400)

#
viewers_line <- viewer_data_long |> 
  ggplot(aes(x = season, y = values)) +
  geom_line(aes(color = viewers)) +
  geom_point(aes(color = viewers)) +
  scale_color_brewer(palette = 'Dark2') +
  theme_classic() +
  scale_y_continuous(labels = scales::unit_format(unit = 'M'), expand = c(0,0)) +
  scale_x_continuous(n.breaks = 40, expand = c(0,0)) +
  labs(
    caption = "https://github.com/akhapwoyaco", 
    title = "US Survivor 40 Seasons Viewership", 
    subtitle = "TidyTuesday: 2021-06-01    https://github.com/doehm/survivoR") +
  theme(
    legend.position = 'inside',
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.position.inside = c(0.6, 0.75),
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.y = element_line()
  )
viewers_line
ggsave(
  plot = viewers_line, filename = "viewers_line.jpeg", 
  width = 35, height = 25, units = "cm", dpi = 400)

#
viewers_facet <- viewer_data_long |>
  ggplot(aes(x = season, y = values, fill = viewers)) +
  geom_col(position = position_dodge()) +
  scale_fill_brewer(palette = 'Dark2') +
  theme_classic() +
  scale_y_continuous(labels = scales::unit_format(unit = 'M'), expand = c(0,0)) +
  scale_x_continuous(n.breaks = 40, expand = c(0,0)) +
  labs(
    caption = "https://github.com/akhapwoyaco", 
    title = "US Survivor 40 Seasons Viewership", 
    subtitle = "TidyTuesday: 2021-06-01    https://github.com/doehm/survivoR") +
  facet_wrap(.~viewers, scales = 'free_y') +
  theme(
    legend.position = 'none',
    # legend.position = 'inside',
    # legend.background = element_blank(),
    # legend.title = element_blank(),
    # legend.position.inside = c(0.6, 0.75),
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.y = element_line()
  )
viewers_facet
ggsave(
  plot = viewers_facet, filename = "viewers_facet.jpeg", 
  width = 35, height = 25, units = "cm", dpi = 400)


# JUDGE VOTES
judge_votes <- summar_y |>
  select(season, final_vote) |>
  separate_longer_delim(cols = final_vote, delim = '-') |>
  group_by(season) |>
  mutate(
    final_vote = as.numeric(final_vote),
    judge_count = row_number() |> as.factor()
    ) |>
  filter(
    final_vote >0
  ) |>
  ggplot(
    aes(x = season, y = final_vote, fill = judge_count)) +
  geom_col() +
  geom_text(
    aes(label = final_vote), vjust = 1.5) +
  scale_y_continuous(n.breaks = 8, expand = c(0,0)) +
  scale_x_continuous(n.breaks = 40, expand = c(0,0)) +
  labs(
    caption = "https://github.com/akhapwoyaco", 
    title = "US Survivor 40 Seasons: Judges Final Voting", 
    subtitle = "TidyTuesday: 2021-06-01    https://github.com/doehm/survivoR") + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
    legend.position = 'none',
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.major = element_blank()
  )
judge_votes  
ggsave(
  plot = judge_votes, filename = "judge_votes.jpeg", 
  width = 35, height = 25, units = "cm", dpi = 400)

### WINNER CHALLENGES
winner_challenges = merge(challenges, summar_y |>
                            select(season, winner, full_name), 
                          by.x = c('season', 'winners'), by.y = c('season', 'winner')) 
winner_challenges_plot = winner_challenges |> 
  group_by(season, challenge_type) |> summarise(n = n()) |>
  ggplot(
    aes(x = season, y = n, fill = challenge_type)
  ) +
  geom_col(position = position_stack()) +
  scale_x_continuous(n.breaks = 40, expand = c(0,0)) +
  labs(
    caption = "https://github.com/akhapwoyaco", 
    title = "US Survivor 40 Seasons: Winner Challenge Counts by Challenge Type", 
    subtitle = "TidyTuesday: 2021-06-01    https://github.com/doehm/survivoR",
    fill = "Challenge Type"
    
    ) + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
    legend.position = 'inside',
    legend.position.inside = c(0.1, 0.8),
    legend.background = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.major = element_blank()
  )
winner_challenges_plot
ggsave(
  plot = winner_challenges_plot, filename = "winner_challenges.jpeg", 
  width = 35, height = 25, units = "cm", dpi = 400)
#
  
  
