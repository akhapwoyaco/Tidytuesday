# get data
tuesdata <- tidytuesdayR::tt_load(2022, week = 3)
chocolate <- tuesdata$chocolate
#
average_rating_plot = chocolate |>
  group_by(company_location) |>
  summarise(
    average_rating = mean(rating)
  ) |>
 ggplot(
   aes(y = average_rating, x = reorder(company_location, -average_rating))
 ) +
  geom_col(fill = 'grey') +
  labs(
    title = "Average Chocolate Rating By Country",
    caption = "https://github.com/akhapwoyaco") +
  geom_text(aes(label = company_location), angle = 90, hjust = 2, color = 'black') +
  theme_void() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = 'bold')
  )
#
ggsave(
  "average_rating_plot.png",plot = average_rating_plot, 
  width = 30, height = 20, unit = "cm", dpi = 550)
#