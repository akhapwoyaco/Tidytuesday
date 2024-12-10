library(readr)
library(RColorBrewer)
# heritage <- read_csv("heritage.csv")
# tuesdata <- tidytuesdayR::tt_load('2024-02-06')
## OR
tuesdata <- tidytuesdayR::tt_load(2024, week = 6)

heritage <- tuesdata$heritage
(heritage)
#
#
png("bar1.png", height=15, width=15, units="cm", res = 300)
barplot(
  as.matrix(heritage[,2:3]), beside=TRUE,
  legend.text=heritage$country,
  args.legend=list(bty="n",horiz=TRUE),
  col=brewer.pal(3,"Set1"),
  border="white",ylim=c(0,20),
  #ylab="",
  main="number of World Heritage sites")
box(bty="l")
dev.off()
#
#
png("bar2.png", height=15, width=15, units="cm", res = 300)
#
barplot(
  as.matrix(heritage[,2:3]), beside=TRUE,horiz=TRUE,
  legend.text=heritage$country, 
  args.legend=list(bty="n"),
  col = brewer.pal(3,"Set1"),border="white",
  xlim=c(0,20), 
  #xlab="",
  main="number of World Heritage sites")
box(bty="l")
dev.off()
#

png("bar3.png", height=15, width=15, units="cm", res = 300)
x = barplot(
  as.matrix(heritage[,2:3]), beside=TRUE,
  legend.text = heritage$country, 
  args.legend=list(bty="n",horiz=TRUE),
  col=brewer.pal(3,"Set1"),border="white",
  ylim=c(0,20),
  main="number of World Heritage sites")
y = as.matrix(heritage[,2:3])
text(x,y-3,labels=as.character(y))
dev.off()
#