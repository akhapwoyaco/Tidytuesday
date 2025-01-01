#
library(tidyverse)
library(dplyr)
library(readxl)
library(janitor)
avia_par_lu_spreadsheet <- read_excel(
  path = "avia_par_lu_spreadsheet.xlsx", 
  sheet = "Sheet 1", skip = 8, na = c(":", "not available" )) |>
  # exclude columns with entirely missing data
  janitor::remove_empty(which = "cols") |>
  drop_na(TIME) |> slice(-1) |> # remove that first row
  filter(!grepl("Special value", TIME))
head(avia_par_lu_spreadsheet)
dim(avia_par_lu_spreadsheet)
#
unique(avia_par_lu_spreadsheet$TIME)
#