#
library(tidyverse)
#
tuesdata <- tidytuesdayR::tt_load(2025, week = 2)
# data
conf2023 <- tuesdata$conf2023
conf2024 <- tuesdata$conf2024
#
library(readr)
conf2023 <- read_csv("conf2023.csv")
head(conf2023)
#
conf2024 <- read_csv("conf2024.csv")
head(conf2024)
#
conf2023$speaker_name
conf2024$speaker_name
#
setdiff(
  x = conf2023$speaker_name, 
  y = conf2024$speaker_name
)
#
words_char_count_2023 = conf2023 |>
  select(
    session_date,
    session_type,
    session_abstract
  ) |> 
  mutate(
    session_date = lubridate::ymd(session_date)
  ) |> 
  mutate(
    session_abstract_chars = nchar(session_abstract),
    session_abstract_words = str_count(session_abstract, "\\W+"),
    session_date = difftime(time1 = session_date, time2 = min(conf2023$session_date), units = "day") |> 
      as.numeric() |> (\(x) x+1)()
  ) |> 
  mutate(
    session_date = factor(session_date)
  ) 

words_char_count_2023 |>
  ggplot() +
  geom_boxplot(
    aes(x = session_type, y = session_abstract_chars, fill = session_date)
  )
#
words_char_count_2023 |>
  ggplot() +
  geom_boxplot(
    aes(x = session_type, y = session_abstract_words, fill = session_date)
  )
#
#