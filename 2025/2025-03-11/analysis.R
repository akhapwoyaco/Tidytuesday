#
library(readr)
pixar_films <- read_csv("pixar_films.csv")
# View(pixar_films)
#
library(ggplot2)
library(ggrepel)
#
pixar_films_plot = pixar_films |>
  ggplot(aes(x = release_date, y = run_time)) +
  geom_point(aes(colour = film_rating)) +
  # Add horizontal axis line
  geom_hline(yintercept = 0, color = "gray30", linetype = "solid", size = 0.7) +
  geom_linerange(mapping = aes(ymin = 0, ymax = run_time)) +
  geom_text_repel(
    aes(label = film, colour = film_rating),
    
    size = 3,
    fontface = "bold",
    direction = "y",
    segment.size = 0.3,
    segment.alpha = 0.6,
    box.padding = 0.5,
    force = 3,
    max.overlaps = Inf,
    segment.inflect = T, min.segment.length = 0,
    segment.curvature = -0.1, segment.linetype = 6,
    nudge_x = .15, arrow = arrow(length = unit(0.015, "npc"))
    
  ) +
  scale_x_date(date_labels = "%m-%y", date_breaks = "6 months") +
  theme_minimal() +
  # theme_economist() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, margin = margin(b = 20)),
    plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 15)),
    plot.margin = margin(t = 20, r = 25, b = 20, l = 20)
  ) +
  
  # Add labels
  labs(
    title = "Pixar Films",
    subtitle = "Release Dates, Ratings and Run-Times",
    x = "Release Date",
    caption = "https://github.com/akhapwoyaco"
  ) + # Add a nice economy-themed watermark (typical of Economist charts)
  annotate(
    "text",
    x = min(pixar_films$release_date) + 10,
    y = -5.5,
    label = "https://github.com/akhapwoyaco",
    color = "gray80",
    size = 10,
    fontface = "bold",
    alpha = 0.2,
    hjust = 0
  )
#
pixar_films_plot
#
ggsave(plot = pixar_films_plot, filename = "pixar_films_plot.jpeg",
      width = 25, height = 20, dpi = 450, units = 'cm')
#
#
#
library(readr)
public_response <- read_csv("public_response.csv")
public_response
pixar_films
#
#
merge(
  pixar_films,
  public_response, all = TRUE
)
