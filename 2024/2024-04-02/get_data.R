#
library(tidyverse)
#
tuesdata <- tidytuesdayR::tt_load(2024, week = 14)
dubois_week10 <- tuesdata$dubois_week10
#
# dubois_week10 <- read.csv("dubois_week10.csv")
(dubois_week10)
#
barplot(dubois_week10)
#
#
library(ggplot2)
prof_dist = dubois_week10 |>
  ggplot(aes(x = reorder(Occupation, -Percentage), y = Percentage)) + 
  geom_col() + 
  geom_text(
    aes(label = Percentage), vjust = 1.2, fontface = 'bold'
  ) + 
  labs(
    title = "The Conditions Of Descendants Of Formal African Slaves Now Resident In The Unites States", 
    subtitle = "Distribution of Professions of Negro Graduates of Atlanta University"
  ) +
  theme_classic() + 
  theme(
    panel.grid.major.y = element_line(),
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, face = 'bold')
  )
prof_dist
ggsave(
  filename = 'professional_distribution.png',
  plot = prof_dist,
  width = 20, height = 15, dpi = 250)

#  
occupation_bar = dubois_week10 |>
  # mutate(
  #   height = "ALL"
  # ) |>
  arrange(desc(Percentage)) |>
  ggplot(aes(
    fill = Occupation,
    x = "ALL",  
    y = Percentage
  )) + 
  geom_col() +#geom_bar(stat = 'identity') + 
  geom_text(
    aes(label = paste(Percentage, '%')),
    position = position_stack(vjust = 0.5)
  ) + scale_fill_brewer(palette = 'Set1') +
  theme_void()
occupation_bar
#
ggsave(
  filename = 'occupation_bar.png',
       plot = occupation_bar,
       width = 40, height = 30, dpi = 500)
#
