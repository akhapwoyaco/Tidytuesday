#
library(readr)
library(dplyr)
#
tuesdata <- tidytuesdayR::tt_load('2018-05-29')
week9_comic_characters <- tuesdata$week9_comic_characters# read_csv("week9_comic_characters.csv")
names(week9_comic_characters)#[6:10]
#' 
#' Unique Publisher
#' 
unique(week9_comic_characters$publisher)
#' 
#' Page Id
unique(week9_comic_characters$name)
#'
 
#' 
#' 

unique(week9_comic_characters$urlslug)
(week9_comic_characters$name)[1]
#
table(week9_comic_characters$sex)

plot1 <- week9_comic_characters |>
  group_by(align, sex) |>
  summarise(count = n()) |> #print()
  arrange(desc(count)) |>
  ggplot(aes(y = reorder(sex, count), x = count, fill = align)) +
  geom_col(position = position_dodge()) + 
  theme_bw() + 
  labs() +
  theme(
    legend.position = 'top', 
    legend.title = element_blank(),
    axis.title = element_blank()
  )
#
plot1
#
ggsave(plot = plot1, filename = "proportionPlot.png", width = 30,
       height = 20, units = "cm", dpi = 350)
#
week9_comic_characters |> names() 
