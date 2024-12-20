#
library(tidyverse)
#
# library(readr)
# datasaurus <- read_csv("datasaurus.csv")
# head(datasaurus)
tuesdata <- tidytuesdayR::tt_load(2020, week = 42)
datasaurus <- tuesdata$datasaurus
#
datasaurus_plot = datasaurus |>
  ggplot() +
  geom_point(aes(x = x, y = y, colour = dataset)) +
  facet_wrap(.~dataset) +
  theme_void() +
  theme(
    legend.position = 'none',
    strip.text = element_text(size = rel(1.5))
  )
#
ggsave(
  "datasaurus.png",plot = datasaurus_plot,
  width = 22, height = 15, unit = "cm", dpi = 450
  )
#