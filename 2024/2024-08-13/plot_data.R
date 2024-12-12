worlds_fairs <- worlds_fairs |> 
  mutate(
    start_date = ym(paste(start_year, start_month, sep='')),
    end_date = ym(paste(end_year, end_month, sep=''))
  ) |> 
  mutate(
    days = end_date - start_date
  )
#
# worlds_fairs |> 
# View()
#
worlds_fairs |> 
  ggplot(aes(x = start_date, y = visitors)) + 
  #geom_point() + #scale_color_identity() +
  geom_linerange(
    aes(x = start_date, ymax = visitors, ymin = 0, 
        colour = category, linewidth = days), 
    #just = 1#,
    position = position_nudge(x = 1),
    stat = "identity"
  ) +
  geom_text(aes(label = country, angle = 90, hjust = -.2)) +
  scale_x_date(
    breaks = worlds_fairs$start_date, date_labels = "%Y-%m"#, expand = c(0,0)
    
  )+
  scale_y_continuous(expand = c(0, 0))+
  theme_classic() + 
  labs(title = "World's Fairs") +
  guides(
    colour = guide_legend(override.aes = list(linewidth = 2))
  ) +
  theme(
    axis.line.y = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.line.x = element_line(lineend = 'square', size = 1),
    #
    axis.text.x = element_text(angle = 90, vjust = 0),
    #
    plot.title = element_text(hjust = 0.5, face = 'bold'), 
    legend.position = "inside", 
    legend.position.inside = c(0.1,0.7)
  )
#

# VISTORS VS DAYS ---------------------------------------------------------
#
worlds_fairs_plot_visitors_days <- worlds_fairs |> 
  mutate(
    country = str_replace(country, pattern = "People's Republic of China", replacement = "People's\nRepublic\nof China")
  ) |>
  ggplot(aes(x = start_date, y = visitors)) + 
  geom_linerange(
    aes(x = start_date, ymax = visitors, ymin = 0, 
        colour = category
    )) +
  geom_text(aes(label = country, angle = 90, hjust = -.2)) +
  scale_x_date(
    breaks = worlds_fairs$start_date[which(!worlds_fairs$visitors %in% c(0.8, 1, NA))],#
    #breaks = worlds_fairs$start_date[which(!is.na(worlds_fairs$visitors))],
    date_labels = "%Y-%m"
  )+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 85), breaks = seq(0, 100, by = 10)) +
  theme_classic() + 
  labs(
    title = "World's Fairs", 
    subtitle = "Number of visitors (millions) and Duration of Fair (days)") +
  guides(
    colour = guide_legend(override.aes = list(linewidth = 3))
  ) +
  geom_segment(aes(x = start_date, xend = end_date, y = visitors, colour = category )) +
  annotate(
    'text', x = as.Date("1880-05-01"), y = 75, 
    label =  'Data : https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-13/worlds_fairs.csv\nGithub: https://github.com/akhapwoyaco/') +
  theme(
    axis.line.y = element_blank(), 
    # axis.text.y = element_blank(), 
    # axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(),
    axis.title = element_blank(),
    axis.line.x = element_line(lineend = 'square', size = 1),
    #
    legend.title = element_blank(),
    
    axis.text.x = element_text(angle = 90, vjust = 0.5, 
                               hjust = 1),
    
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
    legend.position = "inside", 
    legend.position.inside = c(0.1,0.7)
  )
#
worlds_fairs_plot_visitors_days
#
ggsave(
  plot = worlds_fairs_plot_visitors_days, filename = "worlds_fairs_plot_visitors_days.jpeg", 
  width = 40, height = 25, units = "cm", dpi = 650)
#

# VISTORS VS COST ---------------------------------------------------------
# VISTORS VS COST
worlds_fairs_plot_visitors_cost <- worlds_fairs |> 
  mutate(
    country = str_replace(
      country, pattern = "People's Republic of China", 
      replacement = "People's\nRepublic\nof China")
  ) |>
  ggplot(aes(x = start_date, y = visitors)) + 
  geom_point(aes(size = cost, colour = category)) +
  geom_linerange(
    aes(x = start_date, ymax = visitors, ymin = 0, 
        colour = category
    )) +
  geom_text(aes(label = country, angle = 90, hjust = -.2)) +
  scale_x_date(
    breaks = worlds_fairs$start_date[which(!worlds_fairs$visitors %in% c(0.8, 1, NA))],#
    #breaks = worlds_fairs$start_date[which(!is.na(worlds_fairs$visitors))],
    date_labels = "%Y-%m"
  )+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 85), breaks = seq(0, 100, by = 10)) +
  theme_classic() + 
  labs(
    title = "World's Fairs", 
    subtitle = "Number of visitors (millions) and \nCost (USD millions or millions of another currency),exceptions: Brussels International Exposition (1935) and Brussels World's Fair (1958) in BEF,\nParis International Exposition (1937) in FRF, Expo '67 (1967) in CAD, Expo '86 (1986) has \"311 CAD deficit\", Expo '88 (1988) in AUD, Expo 2000 (2000) in DEM") +
  guides(
    colour = guide_legend(override.aes = list(linewidth = 3))
  ) +
  # geom_segment(aes(x = start_date, xend = end_date, y = visitors, colour = category )) +
  annotate(
    'text', x = as.Date("1880-05-01"), y = 75, 
    label =  'Data : https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-13/worlds_fairs.csv\nGithub: https://github.com/akhapwoyaco/') +
  theme(
    axis.line.y = element_blank(), 
    # axis.text.y = element_blank(), 
    # axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(),
    axis.title = element_blank(),
    axis.line.x = element_line(lineend = 'square', size = 1),
    #
    legend.title = element_blank(),
    
    axis.text.x = element_text(angle = 90, vjust = 0.5, 
                               hjust = 1),
    
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
    legend.position = "inside", 
    legend.position.inside = c(0.1,0.7)
  )
#
worlds_fairs_plot_visitors_cost
#
ggsave(
  plot = worlds_fairs_plot_visitors_cost, filename = "worlds_fairs_plot_visitors_cost.jpeg", 
  width = 40, height = 25, units = "cm", dpi = 650)
#