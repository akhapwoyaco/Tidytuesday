#
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load(2025, week = 3)
#
exped_tidy <- tuesdata$exped_tidy
peaks_tidy <- tuesdata$peaks_tidy
#