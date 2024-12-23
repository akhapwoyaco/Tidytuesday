#
# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-09-06')
tuesdata <- tidytuesdayR::tt_load(2022, week = 36)

inventories <- tuesdata$inventories

# Or read in the data manually

inventories <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
#
library(readr)
write_csv(inventories, 'inventories.csv')
write_csv(inventory_sets, 'inventory_sets.csv')
write_csv(sets, 'sets.csv')
#
tuesdata