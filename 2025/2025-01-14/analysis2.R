#
library(ggrepel)
library(ggthemes)
#

sa2023 = conf2023 |>
  select(session_abstract, speaker_name, session_title) |> 
  mutate(
    talk_title = session_title,
    chars = nchar(session_abstract),
    words = str_count(session_abstract, "\\W+"),
    year = 2023
  ) |> select(-session_abstract, -session_title)

ds2024 = conf2024 |> 
  select(description, speaker_name, talk_title)|> 
  mutate(
    chars = nchar(description),
    words = str_count(description, "\\W+"),
    year = 2024) |>
  select(-description)
#
sa2023 |> head()
ds2024 |> head()
#
all_data = bind_rows(
  sa2023, ds2024
) |>
  mutate(
    speaker_name = str_replace_all(speaker_name, c(", " = "\n", "and " = "")),
    talk_title = talk_title |> str_wrap(width = 40)
  ) |>
  mutate(
    labels = ifelse(
      chars > 800 | chars < 350, 
      paste(
        speaker_name, paste0("(", talk_title, ")"),
        sep = "\n"), "")
  ) 
#
#
char_words_plot = all_data |>
  drop_na() |>
  ggplot() +
  geom_point(aes(x = chars, y = words, color = as.factor(year))) +
  geom_text_repel(
    aes(x = chars, y = words,label = labels,color = as.factor(year)),
    box.padding = 1, max.overlaps = Inf, 
    nudge_y = 1, segment.inflect = T, #min.segment.length = 0,
    segment.curvature = -0.1, segment.linetype = 6, 
    nudge_x = .15, arrow = arrow(length = unit(0.015, "npc")),
    xlim = c(200, 2500), ylim = c(50, 300)
  ) +
  labs(
    title = "Posit Conferences 2023-2024: Session Abstracts",
    subtitle = "Counts of the brief description of the talks: Words ~ Character",
    x = "Characters", y = "Words", 
    caption = c("tidytuesdayR::tt_load(2025, week = 2)", "https://github.com/akhapwoyaco/")
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_excel_new() +
  theme(
    axis.text = element_text(face = 'bold'),
    axis.title = element_text(face = 'bold'),
    plot.title = element_text(face = 'bold', hjust = 0.5),
    plot.subtitle = element_text(face = 'bold', hjust = 0.5),
    legend.position = 'inside',
    legend.position.inside = c(0.3, 0.9),
    legend.background = element_blank(),
    legend.direction = 'horizontal',
    plot.caption = element_text(hjust = c(1,0))
  )
#
char_words_plot
#
ggsave(
  filename = "char_words_plot.jpeg", plot = char_words_plot, 
  width = 35, height = 25, units = "cm", dpi = 750
)
# px
ggsave(
  filename = "char_words_plot.jpeg", plot = char_words_plot, 
  width = 6000, height = 5000, units = "px", dpi = 550
)
#
