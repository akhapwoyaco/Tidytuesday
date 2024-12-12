# #
# # Option 1: tidytuesdayR package 
# ## install.packages("tidytuesdayR")
# 
tuesdata <- tidytuesdayR::tt_load('2024-12-31')
# ## OR
# tuesdata <- tidytuesdayR::tt_load(2024, week = 53)
# 
book <- tuesdata$book
broadcast_media <- tuesdata$broadcast_media
journalism <- tuesdata$journalism
leadership <- tuesdata$leadership
restaurant_and_chef <- tuesdata$restaurant_and_chef
# 
# # Option 2: Read directly from GitHub
# 
# book <- readr::read_csv(
#   'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-31/book.csv')
# broadcast_media <- readr::read_csv(
#   'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-31/broadcast_media.csv')
# journalism <- readr::read_csv(
#   'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-31/journalism.csv')
# leadership <- readr::read_csv(
#   'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-31/leadership.csv')
# restaurant_and_chef <- readr::read_csv(
#   'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-31/restaurant_and_chef.csv')

#
table(book$publisher, book$subcategory)
#