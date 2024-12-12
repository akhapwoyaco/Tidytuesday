#
library(tidyverse)
library(readr)
library(ggrepel)
#
# Option 1: tidytuesdayR package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2024-06-18')
## OR
# tuesdata <- tidytuesdayR::tt_load(2024, week = 25)

federal_holidays <- tuesdata$federal_holidays
proposed_federal_holidays <- tuesdata$proposed_federal_holidays

# Option 2: Read directly from GitHub
# federal_holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-06-18/federal_holidays.csv')
# proposed_federal_holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-06-18/proposed_federal_holidays.csv')
#
#
# federal_holidays <- read_csv("federal_holidays.csv")
# # View(federal_holidays)
# #
# proposed_federal_holidays <- read_csv("proposed_federal_holidays.csv")
# View(proposed_federal_holidays)
#
holidays <- bind_rows(
  federal_holidays |> 
    mutate(
      state = 'Federal Holiday'
    ), 
  proposed_federal_holidays |> 
    mutate(
      state = 'Proposed Holiday'
    )
) |> 
  separate_wider_delim(
    cols = date, delim = ' ', names = c('month', 'day_interval'),
    too_few = 'align_start', cols_remove = F
  ) |> 
  separate_wider_delim(
    cols = day_interval, delim = '–', names = c('start', 'end'),
    too_few = 'align_start'
  ) |> 
  mutate(
    start_date = ymd(paste('2024', month, start, sep='')),
    end_date = ymd(paste('2024', month, end, sep='')),
    y = nchar(official_name)
  ) |> select(-start, -end, -month) |> 
  mutate(
    y = case_when(
      state == 'Federal Holiday' ~ y,
      state == 'Proposed Holiday' ~ -y
    )
  )
#
View(holidays)
#
#
holidays_plot <- holidays |> 
  ggplot(
    aes(x = start_date, y = y)
  ) + 
  geom_linerange(
    aes(x = start_date, y = y, ymin = 0, ymax = (\(x) ifelse(x > 0, x-2,x+2))(y) )
  ) +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black') +
  theme_classic() + 
  scale_y_continuous(limits = c(-35,40)) +
  ggrepel::geom_text_repel(
    aes(x = start_date, y = y, 
        label = paste(official_name |> trimws(), date, sep = '\n'), 
        color = state)) + 
  annotate(
    'text', x = as.Date("2024-10-01"), y = 30,
    # /federal_holidays.csv\n
    # Data : https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-18/proposed_federal_holidays.csv\n
    label =  'Data : https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-18\nGithub: https://github.com/akhapwoyaco/') +
  theme(
    axis.line = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    # 
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
    legend.position = "top"
  ) 
#
holidays_plot
ggsave(
  plot = holidays_plot, filename = "holidays_plot.jpeg", 
  width = 40, height = 25, units = "cm", dpi = 400)
#
