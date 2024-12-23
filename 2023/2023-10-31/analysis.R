#
library()
tuesdata <- tidytuesdayR::tt_load(2023, week = 44)
horror_articles <- tuesdata$horror_articles
#
horror_articles |>
  names()
#
horror_articles |>
  group