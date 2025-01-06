#
tuesdata <- tidytuesdayR::tt_load('2019-02-19')
phd_by_field <- tuesdata$phd_by_field
#
head(phd_by_field)
#

table_broad_field = table(phd_by_field$broad_field)
#
# plot(table_broad_field)
#$
jpeg("table_broad_field_bar.jpeg", res = 300, height = 10, width = 15, units = 'cm')
oldp <- par(mgp=c(3, 2, 0))
barplot(
  table_broad_field, horiz = F, names.arg = names(table_broad_field) |> 
    gsub(
      pattern = ' ', replacement = '\n'),
  cex.axis = 0.9, cex.names = 0.8,
  space = 1
)
grid()
par <- oldp
dev.off()
