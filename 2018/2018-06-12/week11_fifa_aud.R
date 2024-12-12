#
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
#' 
#' Data
tuesdata <- tidytuesdayR::tt_load('2018-06-12')
#' 
week11_fifa_audience <- tuesdata$week11_fifa_audience#read_csv("week11_fifa_audience.csv")
week11_fifa_audience$...1 <- NULL
# View(week11_fifa_audience)
#' 
#' 
#' 
plot1 <- week11_fifa_audience |> 
  ggplot(aes(x = tv_audience_share, y = gdp_weighted_share, label = country)) + 
  geom_point() + 
  geom_text_repel(
    data = subset(
      week11_fifa_audience, population_share > 4 | gdp_weighted_share > 5)
  ) + 
  labs(caption = "https://github.com/akhapwoyaco") +
  theme_bw() + 
  facet_grid(
    .~confederation
  )

plot1#
ggsave(plot = plot1, filename = "audience_gdp_1.png", width = 30, height = 20, units = "cm", dpi = 450)
#

unique(week11_fifa_audience$population_share)
sort(unique(week11_fifa_audience$population_share))
sort(unique(week11_fifa_audience$tv_audience_share))
sort(unique(week11_fifa_audience$gdp_weighted_share))
#
week11_fifa_audience$pop_bin <- cut(week11_fifa_audience$population_share, 
    breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 5, Inf),
    include.lowest = TRUE)
#
head(week11_fifa_audience)
# ADD COLUMN OF GDP_weighed sum by confederation
week11_fifa_audience$gdp_weighted_share_by_conf <- ave(
  week11_fifa_audience$gdp_weighted_share, week11_fifa_audience$confederation, 
  FUN = sum
)
plot2 <- week11_fifa_audience |> 
  ggplot(aes(
    x = tv_audience_share, y = gdp_weighted_share, 
    label = country, color = pop_bin)) + 
  geom_point() + 
  geom_text_repel(
    data = subset(
      week11_fifa_audience, population_share > 4 | gdp_weighted_share > 5)
  ) + 
  theme_bw() + 
  labs(caption = "https://github.com/akhapwoyaco") +
  scale_color_brewer(
    palette = 'RdYlBu',
    labels = c(
      "[0,0.5]", "(0.5,1]", "(1,1.5]", "(1.5,2]", "(2,2.5]", 
      "(2.5,3]", "(3,3.5]", "(3.5,5]", "(5,Inf]"
    )
  ) +
  facet_grid(
    .~confederation, scales = 'free_x'
  ) + 
  theme(
    legend.position = 'bottom', legend.direction = 'horizontal',
    legend.title = element_blank()
    ) + 
  guides(color = guide_legend(nrow = 1))
plot2
#
#' 
ggsave(plot = plot2, filename = "audience_gdp.png", width = 30, height = 20, units = "cm", dpi = 450)

#' 
#' Worlds Map
#' 
week11_fifa_audience |>
  group_by(confederation) |>
  summarise(
    s = sum(gdp_weighted_share)
  )
#' 
#' 
#' 
#' 