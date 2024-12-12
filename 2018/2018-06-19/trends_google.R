#
library(readr)
library(janitor) #clean names
library(tidyr)
library(stringr)
#

# Google Trends -----------------------------------------------------------
#
tuesdata <- tidytuesdayR::tt_load('2018-06-19')

#
week12_google_trends <- tuesdata$week12_google_trends |> 
  # read_csv(
  # file = "week12_google_trends.csv", 
  # col_types = cols(Day = col_date(format = "%Y-%m-%d")), 
  # skip = 2) |> 
  clean_names()
  
head(week12_google_trends)
#
library(dplyr)
library(ggplot2)
library(tidyr)
#
week12_google_trends |> 
  pivot_longer(
    cols = !day,
    names_to = 'hurricane',
    values_to = 'values'
  ) |> 
  mutate(
    hurricane = str_replace_all(hurricane, c('hurricane_' = "", 
                                             "_united_states" = ""))
  ) |>
  ggplot(aes(x = day, y = values, color = hurricane)) + 
  geom_point() + geom_line() + 
  theme_minimal() + 
  labs(caption = "https://github.com/akhapwoyaco") +
  scale_x_date(date_labels = '%d-%b-%y', date_breaks = '1 week') +
  theme(
    legend.title = element_blank(),
    legend.position = 'top', 
    axis.title = element_blank()
  )
#  
# MediaCloud --------------------------------------------------------------
#
library(readr)
week12_mediacloud_hurricanes <- tuesdata$week12_mediacloud_hurricanes
  # read_csv(
  # file = "week12_mediacloud_hurricanes.csv",  
  # col_types = cols(Date = col_date(format = "%m/%d/%y")))

head(week12_mediacloud_hurricanes)
#
week12_mediacloud_hurricanes |> 
  mutate(
    Date = mdy(Date)#col_date(format = "%m/%d/%y")
  ) |>
  pivot_longer(
    cols = !Date,
    names_to = 'hurricane',
    values_to = 'values'
  ) |>
  ggplot(aes(x = Date, y = values, color = hurricane)) + 
  geom_point() + geom_line() + 
  theme_minimal() + 
  labs(caption = "https://github.com/akhapwoyaco") +
  scale_x_date(date_labels = '%d-%b-%y', date_breaks = '1 week') +
  theme(
    legend.title = element_blank(),
    legend.position = 'top', 
    axis.title = element_blank()
  )
#
# Mediacloud States -------------------------------------------------------
#
library(readr)
week12_mediacloud_states <- tuesdata$week12_mediacloud_states
  # read_csv(
  # file = "week12_mediacloud_states.csv", 
  # col_types = cols(Date = col_date(format = "%m/%d/%y")))
head(week12_mediacloud_states)
#
week12_mediacloud_states |> 
  mutate(
    Date = mdy(Date)#col_date(format = "%m/%d/%y")
  ) |>
  pivot_longer(
    cols = !Date,
    names_to = 'state',
    values_to = 'values'
  ) |>
  ggplot(aes(x = Date, y = values, color = state)) + 
  geom_point() + geom_line() + 
  theme_minimal() + 
  labs(caption = "https://github.com/akhapwoyaco") +
  scale_x_date(date_labels = '%d-%b-%y', date_breaks = '1 week') +
  theme(
    legend.title = element_blank(),
    legend.position = 'top', 
    axis.title = element_blank()
  )
#
# top online news ---------------------------------------------------------
#
library(readr)
week12_mediacloud_top_online_news <- tuesdata$week12_mediacloud_top_online_news
  # read_csv(
  # file = "week12_mediacloud_top_online_news.csv")
head(week12_mediacloud_top_online_news)

week12_mediacloud_top_online_news |> 
  mutate(
    hyperlink = paste0("<a href='", url, "'>", name, "</a>", sep = '')
  ) |> pull(hyperlink) |> matrix(nrow = 7, ncol = 7) |> 
  datatable(escape = FALSE)
#

# trump -------------------------------------------------------------------
#
library(readr)
week12_mediacloud_trump <- tuesdata$week12_mediacloud_trump |>
  # read_csv(
  # file = "week12_mediacloud_trump.csv", 
  # col_types = cols(Date = col_date(format = "%Y-%m-%d"))) |> 
  clean_names()
head(week12_mediacloud_trump)
#
week12_mediacloud_trump |> 
  pivot_longer(
    cols = !date,
    names_to = 'title',
    values_to = 'values'
  ) |>
  mutate(
    title = str_replace_all(title, c('title_' = ""))
  ) |>
  ggplot(aes(x = date, y = values, color = title)) + 
  geom_point() + geom_line() + 
  theme_minimal() + 
  labs(caption = "https://github.com/akhapwoyaco") +
  scale_x_date(date_labels = '%d-%b-%y', date_breaks = '1 week') +
  theme(
    legend.title = element_blank(),
    legend.position = 'top', 
    axis.title = element_blank()
  )
#
# tv Hurricanes -----------------------------------------------------------
#
library(readr)
week12_tv_hurricanes <- tuesdata$week12_tv_hurricanes
  # read_csv(
  # file = "week12_tv_hurricanes.csv", 
  # col_types = cols(Date = col_date(format = "%m/%d/%y")))
head(week12_tv_hurricanes)
#
week12_tv_hurricanes |> 
  mutate(
    Date = mdy(Date)#col_date(format = "%m/%d/%y")
  ) |>
  pivot_longer(
    cols = !Date,
    names_to = 'hurricane',
    values_to = 'values'
  ) |>
  ggplot(aes(x = Date, y = values, color = hurricane)) + 
  geom_point() + geom_line() + 
  theme_minimal() + 
  labs(caption = "https://github.com/akhapwoyaco") +
  scale_x_date(date_labels = '%d-%b-%y', date_breaks = '1 week') +
  theme(
    legend.title = element_blank(),
    legend.position = 'top', 
    axis.title = element_blank()
  )
#

