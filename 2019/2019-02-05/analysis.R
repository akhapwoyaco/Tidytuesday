#
# get data
tuesdata <- tidytuesdayR::tt_load('2019-02-05')
#
mortgage <- tuesdata$mortgage
recessions <- tuesdata$recessions                                                                                           
state_hpi <- tuesdata$state_hpi
#
head(mortgage)
head(recessions)
head(state_hpi)
#