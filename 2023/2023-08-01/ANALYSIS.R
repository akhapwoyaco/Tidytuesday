library(tidyverse)
tuesdata <- tidytuesdayR::tt_load(2023, week = 31)
states <- tuesdata$states
state_name_etymology <- tuesdata$state_name_etymology

state_name_plot = state_name_etymology |> 
  select(state, date_named) |>
  full_join(
    states |> select(state, n_representatives)
  ) |> 
  ggplot(
    aes(x = date_named, y = n_representatives)
  ) + 
  geom_linerange(
    aes(x = date_named, y = n_representatives, 
        ymin = 0, ymax = (\(x) ifelse(x > 0, x-2,x+2))(n_representatives) )
  ) +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black') +
  theme_classic() + 
  # scale_y_continuous(limits = c(-35,40)) +
  ggrepel::geom_text_repel(
    aes(x = date_named, y = n_representatives, 
        label = state |> trimws(), angle = 90
    )) + 
  labs(
    title = "USA State Names",
    subtitle = "Date on which the name was first attested. (Height is No. of representatives in US House of Representatives)",
    caption = "https://github.com/akhapwoyaco"
  ) + 
  theme(
    axis.line = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    # 
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, face = 'bold')
  )
#
state_name_plot
ggsave(
  plot = state_name_plot, filename = "state_name_plot.jpeg", 
  width = 40, height = 30, units = "cm", dpi = 400)
#
#
state_admission_plot = states |> select(state, admission, n_representatives) |>
  ggplot(
    aes(x = admission, y = n_representatives)
  ) + 
  geom_linerange(
    aes(x = admission, y = n_representatives, 
        ymin = 0, ymax = (\(x) ifelse(x > 0, x-2,x+2))(n_representatives) )
  ) +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black') +
  theme_classic() + 
  # scale_y_continuous(limits = c(-35,40)) +
  ggrepel::geom_text_repel(
    aes(x = admission, y = n_representatives, 
        label = state |> trimws(), angle = 90
    )) + 
  labs(
    title = "USA State Names",
    subtitle = "Date when the state was admitted to the union/ratified the US Constitution. (Height is No. of representatives in US House of Representatives)",
    caption = "https://github.com/akhapwoyaco"
  ) + 
  theme(
    axis.line = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    # 
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, face = 'bold')
  )
#
state_admission_plot
ggsave(
  plot = state_admission_plot, filename = "state_admission_plot.jpeg", 
  width = 40, height = 30, units = "cm", dpi = 400)
#