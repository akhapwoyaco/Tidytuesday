
# The Updated Data From Website -------------------------------------------
#
all_files = list.files(path = 'week10_bikeALL/', full.names = T)
all_files
#
library(purrr)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
#library(magrittr)
#
bike_data <- pmap_dfr(
  .f = fread, 
  .l = list(all_files),
  .id = NULL, #index of file 
  na.strings = "NULL" )
#

colSums(!is.na(bike_data))
colSums(is.na(bike_data))
bike_data <-  bike_data |> 
  mutate(
    StartLatitude = coalesce(StartLatitude, Start_Latitude),
    StartLongitude = coalesce(StartLongitude, Start_Longitude),
    EndLatitude = coalesce(EndLatitude, End_Latitude),
    EndLongitude = coalesce(EndLongitude, End_Longitude),
    Distance_Miles = coalesce(Distance_Miles, Distance_Miles_),
    Duration = lubridate::hms(Duration)) |> 
  select(
    -Start_Latitude, -Start_Longitude, -End_Latitude, 
    -End_Longitude, -Distance_Miles_
  )
#
dim(bike_data)
colSums(!is.na(bike_data))
colSums(is.na(bike_data))
#
str(bike_data)
#
bike_data <- bike_data |>
  unite(
    col = 'EndDateTime',  EndDate:EndTime, sep = ' ', remove = TRUE
  ) |> 
  unite(
    col = 'StartDateTime',  StartDate:StartTime, 
    sep = ' ', remove = TRUE
  ) |> # na.omit()
  mutate(
     EndDateTime = as.POSIXct(
       x = strptime(EndDateTime, format = "%m/%d/%Y %H:%M", tz = 'GMT')),
     StartDateTime = as.POSIXct(
       x = strptime(StartDateTime, format = "%m/%d/%Y %H:%M", tz = 'GMT'))
   ) |> 
  mutate(
    weekday = lubridate::wday(StartDateTime, label = T),
    hour_day = lubridate::hour(StartDateTime)
    ) |> 
  mutate(
    hour_day = case_when(
      hour_day == 0 ~ '12 am',
      hour_day > 0 & hour_day < 12 ~ paste(hour_day, 'am', sep = ' '),
      hour_day == 12 ~ '12 pm',
      hour_day > 12 ~ paste(hour_day-12, 'pm', sep = ' ')
  )
)



# mm = sort(unique(bike_data$hour_day))
# mm |> as_tibble() |> 
#   mutate(
#     hour_day = case_when(
#       value == 0 ~ '12 am',
#       value > 0 & value < 12 ~ paste(mm, 'am', sep = ' '),
#       value == 12 ~ '12 pm',
#       value > 12 ~ paste(mm-12, 'pm', sep = ' ')
#     )
#   ) |> print(n = 25)
# 
bike_data |> select(weekday, hour_day) |> 
  na.omit() |>  group_by(weekday, hour_day) |> 
  summarise(n = n()) |> 
  mutate(
    hour_day = factor(
      hour_day, 
      levels =  c("12 am","1 am","2 am","3 am","4 am","5 am",
                  "6 am","7 am", "8 am", "9 am","10 am","11 am",
                  "12 pm", "1 pm","2 pm","3 pm","4 pm","5 pm",
                  "6 pm","7 pm", "8 pm", "9 pm", "10 pm","11 pm") 
    )
  ) |> 
  ggplot(aes(x = weekday, y = hour_day, fill = n)) + 
    geom_tile(color = 'white', linewidth = 0.1) + 
    geom_text(aes(label = n)) +
    scale_fill_gradient(low = 'yellow', high = 'red') + 
    theme_classic() + 
    theme(
      legend.title = element_blank(),
      legend.position = 'none', axis.title = element_blank()
    )

#


trips_week <- bike_data |> 
  select(StartDateTime) |> 
  mutate(
    StartDate = lubridate::date(bike_data$StartDateTime),
    week = lubridate::week(StartDateTime)
  ) |> select(StartDate, week) |>
  group_by(
    StartDate, week
  ) |> 
  summarise(
    n = n()
  ) |> na.omit() 

trips_week |>
  ggplot(aes(x = StartDate, y = n)) + 
  geom_area(color = 'black', fill = 'blue', alpha = 0.5) +
  theme_minimal() + 
  scale_x_date(date_labels = '%b-%y', date_breaks = '6 months') +
  theme(
    legend.title = element_blank(),
    legend.position = 'none', axis.title = element_blank()
  )
#
mean(bike_data$Duration, na.rm = TRUE)
dim(bike_data)
#
bike_data |> 
  mutate(
    StartDate = lubridate::date(bike_data$StartDateTime)
    ) |>
  group_by(StartDate, StartHub, StartLatitude, StartLongitude) |> 
  summarise(n = n()) |> top_n(20) -> kk
  
library(sf)
library(leaflet)
library(sp)
#
#
kk1 <- kk
# coordinates(kk1) ~StartLongitude+StartLatitude
# #kk1
# #kk1 <- st_as_sf(kk1)
# leaflet(kk1) %>% 
#   addMarkers(lng = ~StartLongitude, lat = ~StartLatitude) %>%  
#   addTiles()

# kk |> 
#   ggplot() +
#   geom_point(
#     aes(x = StartLongitude, y = StartLatitude, size = n))

library(sf)
#library(mapview)
library(rnaturalearth)
library(rnaturalearthdata)
#
world <- ne_countries(scale = 'medium', country = 'United States of America', 
                      returnclass = 'sf')
#
#kk1 %>% 
ggplot(data = world) + 
  geom_sf() + 
  geom_point(data = kk1,
    aes(x = StartLongitude, y = StartLatitude))
 
