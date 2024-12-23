#
temp = list.files(path = '.', pattern = '*.csv')
temp
#
# import all csv and assign their names (excluding .csv) to object 
# in global environment [[based on stackoverflow question]]:
# https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once
for (i in 1:length(temp)){
  assign(
    x = gsub(pattern = "\\.csv", replacement = '', x = temp[i] ),
    read.csv(temp[i]))
}
#
ls()
head(inventories)
head(sets)
head(inventory_sets)
#
library(tidyverse)
all_df <- left_join(inventories, inventory_sets, by = "set_num") |>
  left_join(sets, by = "set_num") 
#
head(all_df)
#
