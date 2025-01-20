#
library(tidyverse)
#
tuesdata <- tidytuesdayR::tt_load(2023, week = 30)
scurvy <- tuesdata$scurvy
#
# View(scurvy)
#
scurvy |> 
  pivot_longer(
    cols = gum_rot_d6:lassitude_d6, 
    names_to = 'symptoms', 
    values_to = 'extent') |> 
  separate_wider_delim(
    cols = extent, 
    delim = "_", 
    names = c('label_no', 'label')) |> 
  separate_wider_delim(
    cols = fit_for_duty_d6, 
    delim = "_", 
    names = c('fit_for_duty_no', 'fit_for_duty_label')) -> mm# |>
  # select(-contains("no")) -> mm
#
mm |> head()
#
plot_frequencies = mm |> 
  mutate(
    label_no = as.numeric(label_no)
  ) |>
  ggplot() +
  geom_col(
    aes(x = treatment, y = label_no+1, fill = symptoms), 
    position = position_dodge()
    ) +
  scale_fill_brewer(palette = 'Dark2')
#
plot_frequencies
ggsave(
"plot_frequencies.jpeg", plot = plot_frequencies,
width = 22, height = 15, unit = "cm", dpi = 450)
#

