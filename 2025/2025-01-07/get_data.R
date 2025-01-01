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
#
#
library(readxl)
#
###########################
# I WANT TO READ ALL SHEETS INEXCEL
#
# setwd() # ALL file_s SHOULD BE IN CURRENT WORKING DIRECTORY

#
library(dplyr)
library(readxl)
# read in file_s in the path
path = "." # data should be in current working directory
file_s <- list.files(path = path, 
                     pattern = ".xlsx$") # Get file_s with .xlsx extension
#
file_s
#
sheet_data_variable = data.frame()
for (file_i in seq_along(file_s)) {
  # Progress check
  print(file_i)
  # get excel sheets
  sheet_nm <- excel_sheets(file_s[file_i]) |>
    str_subset(pattern = "Sheet")
  #
  print(sheet_nm)
  # for each sheet, we read to obtain name 
  for (sheet_name in sheet_nm){
    # get name of series
    un_separated_name_series = read_excel(
      path = "avia_par_lu_spreadsheet.xlsx", 
      sheet = sheet_name, 
      range = "C5:C7", col_names = "title") |> 
      summarise_all(toString)
    name_series = un_separated_name_series |> unlist(use.names = F) |> 
      gsub(pattern = ",|\\(|\\)", replacement = "") |>
      gsub(pattern = " |,", replacement = "_") |> 
      tolower()
    sheet_data_variable = bind_rows(
      sheet_data_variable,
      bind_cols(
        data.frame(sheet = sheet_name, series = name_series),
        un_separated_name_series
      ))
    # import data and assign to the data series name
    print(name_series)
    #
    sheet_data = read_excel(
      path = "avia_par_lu_spreadsheet.xlsx", 
      sheet = sheet_name, skip = 8, na = c(":", "not available" )) |>
      # exclude columns with entirely missing data
      janitor::remove_empty(which = "cols") |>
      drop_na(TIME) |> slice(-1) |> # remove that first row
      filter(!grepl("Special value", TIME))
    assign(
      x = name_series, 
      sheet_data
    )
    rm(sheet_data)
  }
}
#
sheet_data_variable_copy = sheet_data_variable |>
  separate_wider_delim(
    cols = title, delim = ",", 
    names = c(
      "Time frequency", "Unit of measure", 
      "Traffic and transport measurement"
    )
  )
#
datasets_flight = ls()[grepl(pattern = "quarterly|monthly|annual|sheet_data", x = ls())]
datasets_flight
save(file = "flights_data.RData", list = datasets_flight)
#