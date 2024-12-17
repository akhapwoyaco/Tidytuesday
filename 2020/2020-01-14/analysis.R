library(tidyverse)
#
tuesdata <- tidytuesdayR::tt_load(2020, week = 3)
passwords <- tuesdata$passwords |>
  drop_na()
#
head(passwords)
tail(passwords)
#
cat_offline_plot = passwords |>
  ggplot() +
  geom_boxplot(
    aes(x = category, y = offline_crack_sec)
  ) +
  labs(
    caption = "https://github.com/akhapwoyaco",
    y = "Time (seconds)",
    title = "Boxplot: Time to crack offline in seconds by Password Category"
  ) +
  facet_grid(.~category, scales = 'free') +
  theme_light() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = 'bold')
  )
cat_offline_plot
#
ggsave(
  "cat_offline_plot.png",plot = cat_offline_plot, 
  width = 30, height = 15, unit = "cm", dpi = 550)
#
cat_strength_plot = passwords |>
  ggplot() +
  geom_boxplot(
    aes(x = category, y = strength)
  ) +
  facet_grid(.~category, scales = 'free') +
  labs(
    caption = "https://github.com/akhapwoyaco",
    y = "Strength", 
    title = "Boxplot: Password Strength (1:10) by Password Category") +
  theme_light() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = 'bold')
  )
cat_strength_plot
#
ggsave(
  "cat_strength_plot.png",plot = cat_strength_plot, 
  width = 30, height = 15, unit = "cm", dpi = 550)
#



