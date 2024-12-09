#' set directory with files from CollegeScorecard_rawData
#' 
setwd('data/2018/2018-04-02/College_Scorecard_Raw_Data_06102024/')
#' 
#' the period we more interested in are in files MERGEDPERIOD_PP.csv
#' so MERGED2004_05_PP.csv to MERGED2016_17_PP.csv
#' get the pattern, check directory for file matching 
#'
# x <- sprintf("%02d", 4:22)
# y <- sprintf("%02d", 5:23)
# acc_year <- paste(x, y, sep = '_')
# #acc_year
# #
# file_s = c()
# for (i in acc_year){
#   file_s = c(file_s, 
#              grep(pattern = i, x = list.files('.'), value = T))
# }
# file_s # Files matching
#' UPDATED
#' 
file_s = grep(pattern = "^MERGE", x = list.files('.'), value = T)
file_s
#' 
#'  
#' Read in the data
#' na.strings are represented as NULLS
#' data files are bulky, so we only get columns of interest via select argument
#' then add .id as file_s being supplied that gives us new column of 
#' index of file being read, ie our filenames are in vector,
#' so to be read into R via pmap_dfr ie merged via rows, it gives us 
#' period from which data is obtained.
#' ie file 2004_05 is the first file in vector directory, so new id column marks
#' it as 1, and so forth until 12 files ie number 12, after 
#' which we replace the indices 1 to 12 with years
#' 
library(data.table)
library(purrr)
#
state_admission_rate <- pmap_dfr(.f = fread, 
              .l = list(file_s ), 
              .id = 'file_s', #index of file
              na.strings = "NULL",
              select = c("STABBR", "ADM_RATE"))
#
# gsub(pattern = "MERGED|(PP.csv)", "MERGED1996_97_PP.csv", replacement = '')
#
file_vec <- gsub(pattern = "MERGED|(_PP.csv)", file_s, replacement = '')
file_vec <- gsub(pattern = "_", file_vec, replacement = '-')
names(file_vec) <- 1:length(file_vec)
file_vec
#
state_admission_rate <- state_admission_rate |> 
  mutate(
    year = file_vec[as.character(file_s)],
    ADM_RATE = as.numeric(ADM_RATE)
  )
#
library(dplyr)
#
#' We get average admission rates by year and state
state_admission_rate_avg = state_admission_rate |> 
  group_by(year, STABBR) |> 
  summarise(
    average_admission_rates = mean(ADM_RATE, na.rm = TRUE)
  )
#'
#' convert state abbreviations to full names
#' 
state_admission_rate_avg$STABBR <- state.name[
  match(state_admission_rate_avg$STABBR, state.abb)]
## json for python replace
# state_all <- state.name
# names(state_all) <- state.abb
# #
# jsonlite::toJSON(
#   x = tibble(as.data.frame(t(data.frame(state_all))))
# )
#
state_admission_rate_avg <- 
  state_admission_rate_avg[complete.cases(state_admission_rate_avg$STABBR),]
#
# save data 
write_csv(state_admission_rate_avg, 'state_admission_rate_avg.csv')
#