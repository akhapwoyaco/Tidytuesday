#
library(tidyverse)
#
tuesdata <- tidytuesdayR::tt_load('2019-02-19')
phd_by_field <- tuesdata$phd_by_field
#
head(phd_by_field)
#

table_broad_field = table(phd_by_field$broad_field)
#
# plot(table_broad_field)
#$
jpeg("table_broad_field_bar.jpeg", res = 300, height = 10, width = 15, units = 'cm')
oldp <- par(mgp=c(3, 2, 0))
barplot(
  table_broad_field, horiz = F, names.arg = names(table_broad_field) |> 
    gsub(
      pattern = ' ', replacement = '\n'),
  cex.axis = 0.9, cex.names = 0.8,
  space = 1, main = "Frequency of the parent field (highest delineator)"
)
grid()
par <- oldp
dev.off()
#
#

#
phd_by_field_plot = phd_by_field |>
  group_by(broad_field, year) |>
  summarise(
    total_phd = sum(n_phds)
  ) |> drop_na() |>
  ggplot() +
  geom_col(aes(x = year, y = total_phd, fill = broad_field)) +
  scale_fill_brewer(palette = 'Dark2') +
  scale_x_continuous(n.breaks = 8) +
  theme_classic() +
  labs(
    fill = "Broad Field",
    y = "Total PhDs",
    main = "Total number of PhDs awarded by The parent field",
    caption = "https://github.com/akhapwoyaco"
  ) +
  theme(
    axis.title.x = element_blank(),
    legend.position = 'inside',
    legend.position.inside = c(0.2, 0.9)
  )
#
ggsave(
  "phd_by_field_plot.jpeg", plot = phd_by_field_plot, 
  width = 22, height = 15, unit = "cm", dpi = 450)
#
