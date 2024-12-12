#
library(readr)
library(stringr)
# tuesdata <- tidytuesdayR::tt_load('2024-01-23')
## OR
tuesdata <- tidytuesdayR::tt_load(2024, week = 4)

english_education <- tuesdata$english_education


# english_education <- read_csv("english_education.csv")
head(english_education)
#
income_dep_town_plot <- english_education |>
  group_by(size_flag, income_flag) |>
  summarise(
    n = n()
  ) |> drop_na() |>
  
  mutate(
    freq = 100*n/sum(n),
    income_flag = str_replace_all(
      income_flag, 
      c("towns" = "")) |> trimws()
  ) |>
  mutate(
    income_flag = str_replace_all(
      income_flag, 
      c("Cities" = "Higher deprivation" ))
  ) |>
  ggplot(
    aes(x = freq, y = size_flag, fill = income_flag)
  ) +
  geom_col() +
  scale_fill_brewer(palette = 'Dark2') +
  labs(title = "Income Deprivation Groups By Town Size, England") +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.x = element_line(),
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.title = element_blank(),
    plot.title = element_text(face = 'bold', hjust = 0.5)
  )
#
ggsave(
  plot = income_dep_town_plot, filename = "income_deprivation_plot.jpeg",  
  width = 30, height = 25, units = "cm", dpi = 400)
#
education_score_town_size = english_education |>
  ggplot(aes(x = education_score, y = size_flag)) +
  geom_violin(color = 'blue') +
  geom_jitter(color = 'blue') +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.x = element_line(),
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.title = element_blank(),
    plot.title = element_text(face = 'bold', hjust = 0.5)
  )
ggsave(
  plot = education_score_town_size, filename = "education_score_town_size.jpeg",  
  width = 25, height = 35, units = "cm", dpi = 400)

