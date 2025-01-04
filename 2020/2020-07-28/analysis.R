#
#
tuesdata <- tidytuesdayR::tt_load(2020, week = 31)
penguins <- tuesdata$penguins
penguins_raw <- tuesdata$penguins_raw
#
head(penguins)
#
library(ggplot2)
#
penguins |>
  ggplot(aes(x = bill_length_mm, flipper_length_mm, colour = island)) +
  geom_point() + 
  geom_smooth(
    method="lm", se=T)+ 
  scale_color_brewer(palette = 'Dark2') +
  theme_light() +
  labs(
    color = "Island", x = "Bill Length (mm)",
    y = "Flipper Length (mm)"
  ) +
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.15, 0.85),
    legend.background = element_blank()
  )
#
#
bill_len_body_mass = penguins |>
  ggplot(aes(x = bill_length_mm, y = body_mass_g, colour = island)) +
  geom_point() + 
  geom_smooth(
    method="lm", se=T)+ 
  scale_color_brewer(palette = 'Dark2') +
  theme_light() +
  labs(
    color = "Island", x = "Bill Length (mm)",
    y = "Body Mass (g)"
  ) +
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.15, 0.85),
    legend.background = element_blank()
  )
#
table(penguins$species, penguins$island)
#

ggsave(
  "bill_len_body_mass.png",plot = bill_len_body_mass, 
  width = 22, height = 15, unit = "cm", dpi = 450)
