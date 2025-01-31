#
water_insecurity_2022 <- readr::read_csv('water_insecurity_2022.csv')
water_insecurity_2023 <- readr::read_csv('water_insecurity_2023.csv')
#
head(water_insecurity_2022)
head(water_insecurity_2023)
#
library(dplyr)
library(tidyr)
library(stringr)
#
water_insecurity = bind_rows(
  water_insecurity_2022 |> select(-geometry),
  water_insecurity_2023 |> select(-geometry)
) |> 
  separate_wider_delim(
    cols = name, delim = ", ", names = c("county", "state")
  ) |> 
  mutate(
    county = str_replace(county, "County", "") |> trimws(),
    state_abb = state.abb[match(state, state.name)]
  ) 
#
water_insecurity |> head(10)
#
dir.create('data')
ls()
#
for (state_abb_c in state.abb){
  print(state_abb_c)
  # append to specific country
  if (!exists(state_abb_c)){
    assign(state_abb_c, data.frame(), envir = .GlobalEnv)
  } 
  
  
  assign(
    state_abb_c,
    rbind(
      get(state_abb_c),
      water_insecurity |>
        dplyr::filter(grepl(pattern = state_abb_c, x = state_abb)) |>
        select(-state)
    ),
    envir = .GlobalEnv)
  #
  saveRDS(
    get(state_abb_c),
    file = paste0("data/", state_abb_c, ".rds"))
  
  rm(state_abb_c)
}
WY
warnings()
# state.abb
rm(list = ls())
ls()
# state.name
# state.region
#