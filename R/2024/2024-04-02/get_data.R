#
dubois_week10 <- read.csv("D:/2024/October/October_5/TidyTuesday/data/2024/2024-04-02/dubois_week10.csv")
(dubois_week10)
#
barplot(dubois_week10)
#
#
library(ggplot2)
dubois_week10 |>
  ggplot(aes(x = reorder(Occupation, -Percentage), y = Percentage)) + 
  geom_col() + 
  geom_text(
    aes(label = Percentage), vjust = 1.2, fontface = 'bold'
  ) + 
  labs(
    title = "The Conditions Of Descendants Of Formal African Slaves Now Resident In The Unites States", 
    subtitle = "Distribution of Professions of Negro Graduates of Atlanta University"
  ) +
  theme_classic() + 
  theme(
    panel.grid.major.y = element_line(),
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, face = 'bold')
  )
#  
dubois_week10 |>
  # mutate(
  #   height = "ALL"
  # ) |>
  arrange(desc(Percentage)) |>
  ggplot(aes(
    fill = Occupation,
    x = "ALL",  
    y = Percentage
    )) + 
  geom_col() +#geom_bar(stat = 'identity') + 
  geom_text(
    aes(label = paste(Percentage, '%')),
    position = position_stack(vjust = 0.5)
  ) + scale_fill_brewer(palette = 'Set1') +
  theme_void()
#
